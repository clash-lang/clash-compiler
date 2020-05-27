{-|
  Copyright   :  (C) 2020 QBayLogic
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Blackbox implementations for functions in "Clash.Sized.Vector".
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Primitives.Sized.Vector where

import           Control.Monad.State                (State, zipWithM)
import qualified Control.Lens                       as Lens
import           Data.Either                        (rights)
import qualified Data.IntMap                        as IntMap
import           Data.Semigroup.Monad               (getMon)
import qualified Data.Text                          as Text
import qualified Data.Text.Lazy                     as LText
import           Data.Text.Lazy                     (pack)
import           Data.Text.Prettyprint.Doc.Extra
  (Doc, string, renderLazy, layoutPretty, LayoutOptions(..),
   PageWidth(AvailablePerLine))
import           Text.Trifecta.Result               (Result(Success))
import qualified Data.String.Interpolate            as I
import qualified Data.String.Interpolate.Util       as I

import           Clash.Backend
  (Backend, hdlTypeErrValue, expr, mkUniqueIdentifier, blockDecl)
import           Clash.Core.Type
  (Type(LitTy), LitTy(NumTy), coreView)
import           Clash.Netlist.BlackBox             (isLiteral)
import           Clash.Netlist.BlackBox.Util        (renderElem)
import           Clash.Netlist.BlackBox.Parser      (runParse)
import           Clash.Netlist.BlackBox.Types
  (BlackBoxFunction, BlackBoxMeta(..), TemplateKind(TExpr, TDecl),
   Element(Component, Typ, TypElem, Text), Decl(Decl), emptyBlackBoxMeta)
import           Clash.Netlist.Types
  (Identifier, TemplateFunction, BlackBoxContext, HWType(Vector),
   Declaration(..), Expr(BlackBoxE, Literal, Identifier), Literal(NumLit),
   BlackBox(BBTemplate, BBFunction), TemplateFunction(..), WireOrReg(Wire),
   Modifier(Indexed, Nested), bbInputs, bbResult, emptyBBContext, tcCache,
   bbFunctions)
import           Clash.Netlist.Id                   (IdType(Basic))
import           Clash.Netlist.Util                 (typeSize)

import           Clash.Util                         (HasCallStack, curLoc)

data FCall =
  FCall
    Identifier -- left
    Identifier -- right
    Identifier -- result

-- | Calculates the number of function calls needed for an evaluation of
-- 'Clash.Sized.Vector.fold', given the length of the vector given to fold.
foldFunctionPlurality :: HasCallStack => Int -> Int
foldFunctionPlurality 1 = 0
foldFunctionPlurality 2 = 1
foldFunctionPlurality n
  | n <= 0 = error $ "functionPlurality: unexpected n: " ++ show n
  | otherwise =
      let (d, r) = n `divMod` 2 in
      1 + foldFunctionPlurality d + foldFunctionPlurality (d+r)

foldBBF :: HasCallStack => BlackBoxFunction
foldBBF _isD _primName args _resTy = do
  tcm <- Lens.use tcCache
  pure (Right (meta tcm, bb))
 where
  bb = BBFunction "Clash.Primitives.Sized.Vector.foldTF" 0 foldTF
  [_, vecLengthMinusOne] = rights args
  vecLength tcm =
    case coreView tcm vecLengthMinusOne of
      (LitTy (NumTy n)) -> n + 1
      vl -> error $ "Unexpected vector length: " ++ show vl
  funcPlural tcm = foldFunctionPlurality (fromInteger (vecLength tcm))
  meta tcm = emptyBlackBoxMeta {bbKind=TDecl, bbFunctionPlurality=[(0, funcPlural tcm)]}

-- | Type signature of function we're generating netlist for:
--
--   fold :: (a -> a -> a) -> Vec (n + 1) a -> a
--
-- The implementation promises to create a (balanced) tree structure.
foldTF :: TemplateFunction
foldTF = TemplateFunction [] (const True) foldTF'

foldTF' :: forall s . (HasCallStack, Backend s) => BlackBoxContext -> State s Doc
foldTF' bbCtx@(bbInputs -> [_f, (vec, vecType@(Vector n aTy), _isLiteral)]) = do
  -- Create an id for every element in the vector
  vecIds <- mapM (\i -> mkId ("acc_0_" <> show i)) [0..n-1]

  vecId <- mkId "vec"
  let vecDecl = sigDecl vecType Wire vecId
      vecAssign = Assignment vecId vec
      elemAssigns = zipWith Assignment vecIds (map (iIndex vecId) [0..])
      resultId =
        case bbResult bbCtx of
          (Identifier t _, _) -> t
          _ -> error "Unexpected result identifier"

  -- Create a list of function calls to be made (creates identifiers for
  -- intermediate result signals)
  (concat -> fCalls, result) <- mkTree 1 vecIds

  let intermediateResultIds = concatMap (\(FCall l r _) -> [l, r]) fCalls
      wr = case IntMap.lookup 0 (bbFunctions bbCtx) of
             Just ((_,rw,_,_,_,_):_) -> rw
             _ -> error "internal error"
      sigDecls = zipWith (sigDecl aTy) (wr:replicate n Wire ++ repeat wr)
                                       (result : intermediateResultIds)
      resultAssign = Assignment resultId (Identifier result Nothing)

  callDecls <- zipWithM callDecl [0..] fCalls
  foldNm <- mkId "fold"

  getMon $ blockDecl foldNm $
    resultAssign :
    vecAssign :
    vecDecl :
    elemAssigns ++
    sigDecls ++
    callDecls

 where
  mkId :: String -> State s Identifier
  mkId = mkUniqueIdentifier Basic . Text.pack

  callDecl :: Int -> FCall -> State s Declaration
  callDecl fSubPos (FCall a b r) = do
    rendered0 <- string =<< (renderElem bbCtx call <*> pure 0)
    let layout = LayoutOptions (AvailablePerLine 120 0.4)
        rendered1 = renderLazy (layoutPretty layout rendered0)
    pure (
      BlackBoxD
        "__FOLD_BB_INTERNAL__"
        [] [] []
        (BBTemplate [Text rendered1])
        (emptyBBContext "__FOLD_BB_INTERNAL__")
        )
   where
    call  = Component (Decl fPos fSubPos (resEl:aEl:[bEl]))
    elTyp = [TypElem (Typ (Just vecPos))]
    resEl = ([Text (LText.fromStrict r)], elTyp)
    aEl   = ([Text (LText.fromStrict a)], elTyp)
    bEl   = ([Text (LText.fromStrict b)], elTyp)

  -- Argument no. of function
  fPos = 0

  -- Argument no. of vector
  vecPos = 1

  -- Create the whole tree
  mkTree
    :: Int
    -- ^ Current level
    -> [Identifier]
    -- ^ Elements left to process
    -> State s ( [[FCall]]    -- function calls to be rendered
               , Identifier -- result signal
               )
  mkTree _lvl []  = error "Unreachable?"
  mkTree _lvl [res] = pure ([], res)
  mkTree lvl results0  = do
    (calls0, results1) <- mkLevel (lvl, 0) results0
    (calls1, result) <- mkTree (lvl+1) results1
    pure (calls0 : calls1, result)

  -- Create a single layer of a tree
  mkLevel
    :: (Int, Int)
    -- ^ (level, offset)
    -> [Identifier]
    -> State s ([FCall], [Identifier])
  mkLevel (!lvl, !offset) (a:b:rest) = do
    c <- mkId ("acc_" <> show lvl <> "_" <> show offset)
    (calls, results) <- mkLevel (lvl, offset+1) rest
    pure (FCall a b c:calls, c:results)
  mkLevel _lvl rest =
    pure ([], rest)

  -- Simple wire without comment
  sigDecl :: HWType -> WireOrReg -> Identifier -> Declaration
  sigDecl typ rw nm = NetDecl' Nothing rw nm (Right typ) Nothing

  -- Index the intermediate vector. This uses a hack in Clash: the 10th
  -- constructor of Vec doesn't exist; using it will be interpreted by the
  -- HDL backends as vector indexing.
  iIndex :: Identifier -> Int -> Expr
  iIndex vecId i = Identifier vecId (Just (Indexed (vecType, 10, i)))

foldTF' args =
  error $ "Unexpected number of arguments: " ++ show (length (bbInputs args))

indexIntVerilog ::  BlackBoxFunction
indexIntVerilog _isD _primName args _ty = return ((meta,) <$> bb)
 where
  meta = emptyBlackBoxMeta{bbKind=bbKi}

  bbKi = case args of
    [_nTy,_aTy,_kn,_v,Left ix]
      | isLiteral ix -> TExpr
    _ -> TDecl

  bb = case args of
    [_nTy,_aTy,_kn,_v,Left ix] | isLiteral ix ->
      Right (BBFunction "Clash.Primitives.Sized.Vector" 0 indexIntVerilogTF)
    _ ->
      BBTemplate <$> case runParse (pack (I.unindent bbText)) of
        Success t -> Right t
        _         -> Left "internal error: parse fail"

  bbText = [I.i|
    // index begin
    ~IF~SIZE[~TYP[1]]~THENwire ~TYPO ~GENSYM[vecArray][0] [0:~LIT[0]-1];
    genvar ~GENSYM[i][2];
    ~GENERATE
    for (~SYM[2]=0; ~SYM[2] < ~LIT[0]; ~SYM[2]=~SYM[2]+1) begin : ~GENSYM[mk_array][3]
      assign ~SYM[0][(~LIT[0]-1)-~SYM[2]] = ~VAR[vecFlat][1][~SYM[2]*~SIZE[~TYPO]+:~SIZE[~TYPO]];
    end
    ~ENDGENERATE
    assign ~RESULT = ~SYM[0][~ARG[2]];~ELSEassign ~RESULT = ~ERRORO;~FI
    // index end|]

indexIntVerilogTF :: TemplateFunction
indexIntVerilogTF = TemplateFunction used valid indexIntVerilogTemplate
 where
  used  = [1,2]
  valid = const True

indexIntVerilogTemplate
  :: Backend s
  => BlackBoxContext
  -> State s Doc
indexIntVerilogTemplate bbCtx = getMon $ case typeSize vTy of
  0 -> hdlTypeErrValue rTy
  _ -> case vec of
    Identifier i mM -> case mM of
      Just m ->
           expr False (Identifier i (Just (Nested m (Indexed (vTy,10,ixI ix)))))
      _ -> expr False (Identifier i (Just (Indexed (vTy,10,ixI ix))))
    _ -> error ($(curLoc) ++ "Expected Identifier: " ++ show vec)
 where
  [  _kn
   , (vec, vTy, _)
   , (ix, _, _)
   ] = bbInputs bbCtx

  (_,rTy) = bbResult bbCtx

  ixI :: Expr ->  Int
  ixI ix0 = case ix0 of
    Literal _ (NumLit i) ->
      fromInteger i
    BlackBoxE "GHC.Types.I#" _ _ _ _ ixCtx _ ->
      let (ix1,_,_) = head (bbInputs ixCtx)
      in  ixI ix1
    _ ->
      error ($(curLoc) ++ "Unexpected literal: " ++ show ix)
