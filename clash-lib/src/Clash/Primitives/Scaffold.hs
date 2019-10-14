{-

If you don't want to provide a Haskell implementation to a HDL function,
you can use 'makeScaffold' to create a HDL blackbox with "stub" Haskell function.
An input datatype of 'Clock's and 'BitVector's with a smart constructor taking
associated clocks and setting the bitvectors to 0, and an output datatype will be
created. Each list of 'Port's is designated to be in a different clock domain.
The datatypes will be parametrized on these clock domains.

An example, a Xilinx IBUFDS_GTE2 primitive, with added dummy signals to show expansion:

@
makeScaffold "xilinxDiffClock" "IBUFDS_GTE2"
  -- A list of parameters
  [ PBool "CLKRCV_TRST" True
  ]

  -- A list of list of ports, corresponding to domains of signals
  -- Clocks will be lifted to the top of defitions
  [ [ ClkOut "O"
    , ClkIn "I"
    , In "dummy_signal1" 8
    , ClkIn "IB"
    ]
  , [ In "dummy_signal2" 40
    , Out "dummy_out1" 1
    ]
  ]
@

Creates a synthesizable HDL blackbox representation and these Haskell values:

@
data XilinxDiffClockI dom1 dom2
  = XilinxDiffClockI
  { _I :: Clock dom1
  , _IB :: Clock dom1
  , _dummy_signal1 :: Signal dom1 (BitVector 8)
  , _dummy_signal2 :: Signal dom2 (BitVector 40)
  }
data XilinxDiffClockO dom1 dom2
  = XilinxDiffClockI
  { _O :: Clock dom1
  , _dummy_out1 :: Signal dom2 (BitVector 1)
  }

-- Smart constructor taking only the clocks
xilinxDiffClockI arg1 arg2 = XilinxDiffClockI arg1 arg2 (pure def) (pure def)

-- Haskell name tied to HDL instantiation
xilinxDiffClock# arg1 arg2 arg3 arg4
  = XilinxDiffClockO clockGen (pure def)

-- A convenience function taking the input data type and calling the blackbox
xilinxDiffClock (XilinxDiffClockI arg1 arg2 arg3 arg4)
  = xilinxDiffClock# arg1 arg2 arg3 arg4
@

-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveLift          #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Clash.Primitives.Scaffold
  ( makeScaffold
  , makeScaffoldWith
  , Port (..)
  , Parameter(..)

  -- Data types for custom record naming
  , ScaffoldDesc(..)
  , ScaffoldPort(..)
  )
  where

import           Prelude

import           Language.Haskell.TH
import           GHC.Generics                   ( Generic )
import           Data.Char                      ( toUpper)
import qualified Data.Text                     as T
import           Data.List                      ( sortOn )
import           Data.Semigroup.Monad           ( getMon )
import           Control.Monad.State            ( State )

import qualified Data.String.Interpolate       as I

import           Clash.Netlist.Id               ( IdType(Basic) )
import           Clash.Netlist.Types     hiding ( PortDirection(..) )
import           Clash.Netlist.Types            ( PortDirection )
import qualified Clash.Netlist.Types           as Netlist
import           Data.Text.Prettyprint.Doc.Extra
                                                ( Doc )
import           Clash.Annotations.Primitive
import           Clash.Backend                  ( Backend
                                                , mkUniqueIdentifier
                                                , blockDecl
                                                )

import qualified Clash.Prelude                 as C
                                         hiding ( Text )
import           Clash.Prelude                  ( Lift
                                                , def
                                                , KnownNat, natVal
                                                )

import           Control.Lens                   ( view
                                                , _1
                                                )

type Width = Integer


-- | A named parameter to an HDL blackbox
data Parameter
  = StringParameter String String
  | BoolParameter String Bool
  | IntegerParameter String Integer
  | forall n. KnownNat n => BitVectorParameter String (C.BitVector n)
  -- deriving Lift
  --
deriving instance Lift Parameter

-- | A named port to an HDL blackbox
data Port
  = In String Width
  | Out String Width
  | ClkIn String
  | ClkOut String

-- * Internal types

type IsClock = Bool

data ScaffoldPort
  = ScaffoldPort
  { direction  :: PortDirection
  , width      :: Width
  , name       :: String
  , isClock    :: IsClock
  , ty         :: Type
  , domain     :: Name
  } deriving (Show, Lift)

data ScaffoldDesc
  = ScaffoldDesc
  { functionName  :: Name
  , functionName# :: Name
  , functionNameI :: Name
  , qfunctionName :: Name
  , qfunctionName# :: Name

  , datatypeNameI :: Name
  , datatypeNameO :: Name

  , templateName  :: Name
  , qtemplateName :: Name

  , primitive     :: String
  } deriving (Show, Lift)

-- * Helper functions for translating between Haskell / Clash / TemplateHaskell


toClashParameter :: Parameter -> (Expr,HWType,Expr)
toClashParameter (StringParameter n v) =
    (Identifier (T.pack n) Nothing, String, Literal Nothing (StringLit v))
toClashParameter (IntegerParameter n v) =
    (Identifier (T.pack n) Nothing, Integer, Literal Nothing (NumLit v))
toClashParameter (BoolParameter n v) =
    (Identifier (T.pack n) Nothing, Bool, Literal Nothing (BoolLit v))
toClashParameter (BitVectorParameter n v) =
    ( Identifier (T.pack n) Nothing
    , BitVector (fromInteger s)
    , Literal Nothing (BitVecLit s (toInteger v)))
   where
    s = natVal v

toTemplateHaskellType :: IsClock -> Width -> Name -> Type
toTemplateHaskellType True _ dom  = AppT (ConT ''C.Clock) (VarT dom)
toTemplateHaskellType False w dom =
  AppT
    (AppT (ConT ''C.Signal) (VarT dom))
    (AppT (ConT ''C.BitVector) (LitT $ NumTyLit w))

toClashType :: ScaffoldPort -> HWType
toClashType (ScaffoldPort _ _ _ True _ _) = Clock (T.pack "clk")
toClashType (ScaffoldPort _ w _ False _ _) = BitVector (fromInteger w)

scaffoldPort :: Name -> Port -> ScaffoldPort
scaffoldPort d (In n w) =
  ScaffoldPort Netlist.In w n False (toTemplateHaskellType False w d) d
scaffoldPort d (Out n w) =
  ScaffoldPort Netlist.Out w n False (toTemplateHaskellType False w d) d
scaffoldPort d (ClkIn n) =
  ScaffoldPort Netlist.In 1 n True (toTemplateHaskellType True 1 d) d
scaffoldPort d (ClkOut n) =
  ScaffoldPort Netlist.Out 1 n True (toTemplateHaskellType True 1 d) d

scaffoldDomain :: [Port] -> Q (Name, [ScaffoldPort])
scaffoldDomain ps = do
  d <- newName "domain"
  return (d, scaffoldPort d <$> ps)

instantiate
  :: ScaffoldPort
  -> Expr
  -> (Expr, PortDirection, HWType, Expr)
instantiate p@(ScaffoldPort dir _ n _ _ _) e
  = (Identifier (T.pack n) Nothing, dir, toClashType p, e)

-- *

-- | Build a Clash-Haskell representation of an HDL primitive
scaffoldTemplate
  :: (Backend s)
  => String
  -> [Parameter]
  -> [Name]
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> BlackBoxContext
  -> State s Doc
scaffoldTemplate primitiveName parameters domains i o bbCtx = do
  wires <- mapM (ident . T.pack . name) o
  inst  <- ident (T.pack $ primitiveName <> "_inst")
  blk   <- ident (T.pack $ primitiveName <> "_blk")

  getMon $ blockDecl blk $ concat
    [ zipWith (NetDecl Nothing) wires wiresTy
    , [ InstDecl
        Comp
        Nothing
        (T.pack primitiveName)
        inst
        (toClashParameter <$> parameters)
        (  zipWith instantiate i (fmap (view _1) args)
        <> zipWith instantiate o (flip Identifier Nothing <$> wires)
        )
      , result wires (bbResult bbCtx)
      ]
    ]
 where
  args    = drop (length domains) (bbInputs bbCtx)
  ident   = mkUniqueIdentifier Basic
  wiresTy = fmap toClashType o

  result ws (Identifier r Nothing, resTy@Product{})
    = Assignment r
    (DataCon resTy (DC (resTy, 0)) [ Identifier w Nothing | w <- ws ])
  result ws (Identifier r Nothing, _) | [wire] <- ws
    = Assignment r (Identifier wire Nothing)
  result _ t = error $ "scaffoldTemplate: unexpected result type"
                     ++ show t

-- TODO: doc
scaffoldTF
  :: [Int]
  -> String
  -> [Parameter]
  -> [Name]
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> TemplateFunction
scaffoldTF used primitiveName parameters domains i o = TemplateFunction
  used
  (const True)
  (scaffoldTemplate primitiveName parameters domains i o)

-- | Annotations to associate our 'TemplateFunction's with our Haskell
-- functions.
scaffoldAnnotation :: Name -> Name -> HDL -> Q Exp
scaffoldAnnotation n ntf hdl =
  [|InlinePrimitive hdl j|]
 where
  j = [I.i| [{ "BlackBox" :
              { "name" : "#{n}"
              , "kind": "Declaration"
              , "format": "Haskell"
              , "templateFunction" : "#{ntf}"
              }
          }]
      |]

pureDefault :: Exp
pureDefault = VarE 'pure `AppE` VarE 'def

knownDomains :: [Name] -> [Type]
knownDomains = fmap (AppT (ConT ''C.KnownDomain) . VarT)

applyDomains :: [Name] -> Type -> Type
applyDomains = flip (foldl AppT) . fmap VarT

-- | Builds the Haskell datatypes "datatypeNameI" and "datatypeNameO",
-- both of which are parametric on the domains of the ports.
makeDatatypes
  :: ScaffoldDesc
  -> ([TyVarBndr],[Name])
  -> ([ScaffoldPort], [Name])
  -> ([ScaffoldPort], [Name])
  -> [Dec]
makeDatatypes desc (kinds,domains) (i,ni) (o,no) =
  [ build (datatypeNameI desc) (zipWith mkRec i ni)
  , build (datatypeNameO desc) (zipWith mkRec o no)

  , SigD (functionNameI desc)
    $ ForallT kinds (knownDomains domains)
    $ foldr (AppT . AppT ArrowT) retTy (ty <$> iclks)
  , FunD (functionNameI desc) [Clause (VarP <$> argNames) (NormalB (
      foldl AppE
        (foldl AppE (ConE (datatypeNameI desc)) (VarE <$> argNames))
        (replicate (length i - length iclks) pureDefault)
      )) []]
  ]
 where
  b = Bang NoSourceUnpackedness NoSourceStrictness
  deriveGeneric = [DerivClause Nothing [ConT ''Generic]]
  iclks = filter isClock i
  argNames = zipWith (const (mkName . (<>) "clkArg" . show)) iclks [0::Int ..]
  retTy = applyDomains domains $ ConT $ datatypeNameI desc
  mkRec (ScaffoldPort _ _ _ _ t _) n = (n, b, t)
#if MIN_VERSION_template_haskell(2,11,0)
  build nd fields = DataD [] nd kinds Nothing [(RecC nd fields)] deriveGeneric
#else
  build nd fields = DataD [] nd kinds [RecC nd fields] deriveGeneric
#endif

-- | Creates the Clash expressions for the HDL primitives
makeTemplate
  :: ScaffoldDesc
  -> [Parameter]
  -> [Name]
  -> [ScaffoldPort]
  -> [ScaffoldPort]
  -> DecsQ
makeTemplate desc parameters domains i o = do
  blackboxAnn <- do
    annotations <-
      traverse
        (scaffoldAnnotation (qfunctionName# desc) (qtemplateName desc))
        [minBound .. maxBound]
    return $ PragmaD . AnnP (valueAnnotation $ qfunctionName# desc) <$> annotations

  blackboxExpr <-
    [| scaffoldTF
       [length domains .. length domains + length i - 1]
       (primitive desc) parameters domains i o
    |]

  return $
    [ SigD (templateName desc) (ConT ''TemplateFunction)
    , FunD (templateName desc) [Clause [] (NormalB blackboxExpr) []]
    ] <> blackboxAnn

-- | Build the Haskell functions "functionName" and "functionName#", and mark
-- them as `NOINLINE`. "functionName" takes "datatypeNameI" as argument, calling
-- "functionName#" and returning "datatypeNameO". "functionName#" constructs its
-- return value "datatypeNameO" by calling 'clockGen' for Clocks and 'pure'
-- 'def' for 'BitVector's.
makeHaskellFuncs
  :: ScaffoldDesc
  -> ([TyVarBndr], [Name])
  -> ([ScaffoldPort], [Name])
  -> [ScaffoldPort]
  -> [Dec]
makeHaskellFuncs desc (kinds, domains) (i,ni) o =
  [
  -- functionName
    SigD (functionName desc)
    $ ForallT kinds (knownDomains domains)
    $ AppT ArrowT argTy `AppT` retTy
  , FunD (functionName desc) [Clause [TupP [VarP arg]] (NormalB (
      foldl AppE (VarE (functionName# desc))
      $ fmap (flip AppE (VarE arg) . VarE) ni
      )) []]
  -- functionName#
  , SigD (functionName# desc)
    $ ForallT kinds (knownDomains domains)
    $ foldr1 (AppT . AppT ArrowT) (fmap ty i ++ [retTy])
  , FunD (functionName# desc) [Clause bangs (NormalB buildOutput) []]
  -- Inline pragmas
  , PragmaD (InlineP (functionName desc) NoInline FunLike AllPhases)
  , PragmaD (InlineP (functionName# desc) NoInline FunLike AllPhases)
  ]
 where
  arg = mkName "_arg"
  bangs = replicate (length i) (BangP WildP)
  buildOutput
      = foldl AppE (ConE (datatypeNameO desc))
      . fmap (\x -> if isClock x then clock else pureDefault)
      $ o
  clock = VarE 'C.clockGen
  retTy = applyDomains domains $ ConT $ datatypeNameO desc
  argTy = applyDomains domains $ ConT $ datatypeNameI desc

-- | 'makeScaffold' but with an explicit record naming strategy. See 'makeScaffold'.
makeScaffoldWith
  :: (ScaffoldDesc -> ScaffoldPort -> Name)
  -> String
  -> String
  -> [Parameter]
  -> [[Port]]
  -> DecsQ
makeScaffoldWith portname nam@(n:ame) primitive' parameters ports' = do
  currLoc <- loc_module <$> location
  let desc = ScaffoldDesc
             { functionName  = mkName nam
             , functionName# = mkName (nam ++ "#")
             , functionNameI = mkName (nam ++ "I")
             , qfunctionName = mkName (currLoc ++ "." ++ nam)
             , qfunctionName#= mkName (currLoc ++ "." ++ nam ++ "#")

             , datatypeNameI = mkName (toUpper n : ame ++ "I")
             , datatypeNameO = mkName (toUpper n : ame ++ "O")

             , templateName  = mkName (nam ++ "TF")
             , qtemplateName = mkName (currLoc ++ "." ++ nam ++ "TF")

             , primitive     = primitive'
             }

  (domains, ports) <- collectDomains <$> mapM scaffoldDomain ports'

  let kinds = flip KindedTV (ConT ''C.Domain) <$> domains

  let i = sortOn (not . isClock) $ filterDir Netlist.In ports
  let o = sortOn (not . isClock) $ filterDir Netlist.Out ports

  let ni = portname desc <$> i
  let no = portname desc <$> o

  mappend (mconcat
   [ makeDatatypes desc (kinds,domains) (i,ni) (o,no)
   , makeHaskellFuncs desc (kinds, domains) (i,ni) o
   ]) <$> makeTemplate desc parameters domains i o
 where
  filterDir dir = filter ((dir==) . direction)
  collectDomains = foldl (\(ns, pss) (n',ps) -> (n':ns,ps<>pss)) ([],[])
makeScaffoldWith _ _ _ _ _ = error "makeScaffoldWith: Empty name given!"

-- | Prefix record names with an underscore
defaultRecordNaming :: ScaffoldDesc -> ScaffoldPort -> Name
defaultRecordNaming _ port = mkName $ "_" <> name port

-- | Instantiates
--   - Input and output datatypes, as well as a smart constructor for the input
--     datatype
--   - A 'TemplateFunction' for the primitive
--   - A function taking the input packed into the input datatype
--   - A function (suffixed with #) taking the individual arguments
--
-- For example:
--
-- @
-- makeScaffold "xilinxDiffClock" "IBUFDS_GTE2"
--   -- A list of parameters
--   [ PBool "CLKRCV_TRST" True
--   ]
--
--   -- A list of list of ports, corresponding to domains of signals
--   -- Clocks will be lifted to the top of defitions
--   [ [ ClkOut "O"
--     , ClkIn "I"
--     , In "dummy_signal1" 8
--     , ClkIn "IB"
--     ]
--   , [ In "dummy_signal2" 40
--     , Out "dummy_out1" 1
--     ]
--   ]
--
--  -- builds ===>
--
-- data XilinxDiffClockI dom1 dom2
--   = XilinxDiffClockI
--   { _I :: Clock dom1
--   , _IB :: Clock dom1
--   , _dummy_signal1 :: Signal dom1 (BitVector 8)
--   , _dummy_signal2 :: Signal dom2 (BitVector 40)
--   }
-- data XilinxDiffClockO dom1 dom2
--   = XilinxDiffClockI
--   { _O :: Clock dom1
--   , _dummy_out1 :: Signal dom2 (BitVector 1)
--   }
--
-- -- Smart constructor taking only the clocks
-- xilinxDiffClockI arg1 arg2 = XilinxDiffClockI arg1 arg2 (pure def) (pure def)
--
-- -- Haskell name tied to HDL instantiation
-- xilinxDiffClock# arg1 arg2 arg3 arg4
--   = XilinxDiffClockO clockGen (pure def)
--
-- -- A convenience function taking the input data type and calling the blackbox
-- xilinxDiffClock (XilinxDiffClockI arg1 arg2 arg3 arg4)
--   = xilinxDiffClock# arg1 arg2 arg3 arg4
-- @
makeScaffold
  :: String -- ^ generated haskell function name
  -> String -- ^ hdl primitive name
  -> [Parameter]
  -> [[Port]]
  -> DecsQ
makeScaffold
  = makeScaffoldWith defaultRecordNaming
