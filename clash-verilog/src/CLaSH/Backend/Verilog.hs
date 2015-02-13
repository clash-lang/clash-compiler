{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Generate VHDL for assorted Netlist datatypes
module CLaSH.Backend.Verilog (VerilogState) where

import           Control.Applicative
import           Control.Lens hiding (Indexed)
-- import           Control.Monad                        (forM,join,liftM,when,zipWithM)
import           Control.Monad.State                  (evalState)
-- import           Data.Graph.Inductive                 (Gr, mkGraph, topsort')
-- import           Data.HashMap.Lazy                    (HashMap)
-- import qualified Data.HashMap.Lazy                    as HashMap
-- import           Data.HashSet                         (HashSet)
import qualified Data.HashSet as HashSet
-- import           Data.List                            (mapAccumL,nubBy)
-- import           Data.Maybe                           (catMaybes,mapMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

-- import           Text.PrettyPrint.Leijen.Text hiding ((<>), (<$>), empty)
import qualified Text.PrettyPrint.Leijen.Text as D
import           Text.PrettyPrint.Leijen.Text.Class

import           Language.Verilog.AST as V
import           Language.Verilog.PrettyPrint ()

import           CLaSH.Backend
import qualified CLaSH.Netlist.BlackBox.Util  as BB
import qualified CLaSH.Netlist.BlackBox.Types as BB
import qualified CLaSH.Netlist.Types as N
import           CLaSH.Util                           (curLoc)

import           CLaSH.Backend.Verilog.BoringTypes as B
import           CLaSH.Backend.Verilog.Bore

#ifdef CABAL
import qualified Paths_clash_verilog
#else
import qualified System.FilePath
#endif

-- | State for the 'CLaSH.Netlist.VHDL.VHDLM' monad:
data VerilogState = VerilogState {}

makeLenses ''VerilogState

instance Backend VerilogState where
  initBackend     = VerilogState
#ifdef CABAL
  primDir         = const (Paths_clash_verilog.getDataFileName "primitives")
#else
  primDir _       = return ("clash-verilog" System.FilePath.</> "primitives")
#endif
  extractTypes    = const HashSet.empty
  name            = const "verilog"
  extension       = const ".v"

  genHDL          = return . ("main",) . pp . toModule . component
  mkTyPackage     = const $ return D.empty
  hdlType         = return . D.text . T.pack . show . toVTy . hwtype
  hdlTypeErrValue = error $ $(curLoc) ++ "not yet implemented"
  hdlTypeMark     = error $ $(curLoc) ++ "not yet implemented"
  inst            = return . Just . pp . toItemBB . declaration
  expr _ e        = return $ pp $ toVExprBB $ CLaSH.Backend.Verilog.Bore.expr e

-- TODO replace orphan instances with implicit arguments?

instance PrettyPrint (BB.BlackBoxTemplate, N.BlackBoxContext) where
  pp (bbT,bbCtx) = let t = evalState (BB.renderBlackBox bbT bbCtx) VerilogState
                   in  D.text t

instance PrettyPrint Text where
  pp = D.text

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pp = \case
    Left a -> pp a
    Right b -> pp b

newtype VExpr iden blackbox = VE (Either blackbox (V.Expr iden (VExpr iden blackbox)))
                            deriving (Show, PrettyPrint)


-- TODO: ?state? monad to bind index'd expressions and index gensym id
toVExpr :: (Int -> expr)
        -> Maybe HWType
        -> CoreExpr (NonIndex expr) t
        -> V.Expr Text expr
toVExpr f t = \case
  E (B.Literal lit)  -> V.Literal (sizeTy <$> t) lit
  E (B.Concat es)    -> V.LValue $ V.Concat $ es
  E (B.Identifier i) -> V.LValue $ V.Identifier i
  -- TODO do something about the inner expr
  B.Index h l _e     -> V.LValue $ V.SubRange _ $ V.Range (f h) (f l)
  E (B.DataCon es)   -> V.LValue $ V.Concat es -- TODO should be struct literal, not concat
  --where tagPrefix = "__clash_variant_tag_"

toVExprBB :: B.Expr blackbox -> VExpr Text blackbox
toVExprBB (MTBBE (MT t e)) = VE $ toVExprBB <$$> toVExpr inj t <$> e
  where inj = MTBBE . MT (Just B.Integer) . Right . E . B.Literal . V.Number . fromIntegral

-- TODO Writer monad to cache types to gen
toVTy :: B.HWType -> V.Type Text
toVTy = \case
  B.Integer          -> error $ $(curLoc) ++ "integers are not synthesizable"
  B.Bits _signed size -> Wire size -- TODO Signed
  B.Vector len ty    -> UnpackedArray (toVTy ty) len
  B.Sum     name' _  -> Nominal name'
  B.Product name' _  -> Nominal name'
  B.SP      name' _  -> Nominal name'
  --Enum name (log $ length ctors) ctors
  -- B.Bits lst -> sum $ fst <$> lst

sizeTy :: B.HWType -> Int
sizeTy = \case
  B.Integer          -> error $ $(curLoc) ++ "Integer is countably infinite -- no fixed number of bits"
  B.Bits _ size      -> size
  B.Vector len ty    -> len * sizeTy ty
  B.Sum     _ ctors  -> ceilingLog ctors
  B.Product _ tys    -> sum $ sizeTy <$> tys
  B.SP      _ tyss   -> (maximum $ sum <$> (sizeTy <$$> snd <$> tyss)) + ceilingLog tyss
  where ceilingLog :: [x] -> Int
        ceilingLog xs = ceiling $ log (fromIntegral $ length xs :: Double)

toItem :: forall blackbox. Declaration blackbox -> ModuleItem Text (VExpr Text blackbox)
toItem = \case
  Assignment is e        -> V.Assign (V.Concat $ idToVE <$> is) $ toVExprBB e
  InstDecl m i  ps       -> V.Instance m [] i $ Just . toVExprBB <$$> ps
  NetDecl i t _me        -> LocalNet (Decl (toVTy t) i) -- , toVExprBB <$> me) -- TODO RHS
  CondAssignment i sc es -> Assign (V.Identifier i) $ conds es
    where
      conds :: [(Maybe (B.Expr blackbox), B.Expr blackbox)] -> (VExpr Text blackbox)
      conds = \case
        []                -> error $ $(curLoc) ++ "CondAssigment cannot have zero branches"
        [(_,e)]           -> toVExprBB e
        ((Nothing,e):_)   -> toVExprBB e
        ((Just c ,e):es') -> injE $ V.Mux (injE $ BinOp Eq (toVExprBB c) (toVExprBB sc)) (toVExprBB e) (conds es')
  where injE = VE . Right
        idToVE e = injE $ LValue $ V.Identifier $ e

type VItem blackbox = Either blackbox (ModuleItem Text (VExpr Text blackbox))

toItemBB :: Either blackbox (Declaration blackbox) -> VItem blackbox
toItemBB = fmap toItem

toModule :: forall blackbox. Component blackbox -> V.Module Text (VItem blackbox)
toModule (Component modName ports ins out decls) = Module modName params items
  where items            = (Right . LocalNet . decl <$> ports)
                           ++ (toItemBB <$> decls)
        params           = mkParam Output out : (mkParam Input <$> ins)
        mkParam dir d    = (dir, decl d)
        decl (param, ty) = V.Decl (toVTy ty) param
