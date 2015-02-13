{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module CLaSH.Backend.Verilog.Bore where

import           Control.Applicative



-- import qualified CLaSH.Netlist.BlackBox.Util as BB
import qualified CLaSH.Netlist.BlackBox.Types as BB
import qualified CLaSH.Netlist.Types as N
import           CLaSH.Util                        (curLoc)

import           Language.Verilog.AST (SizedLiteral(..))

import           CLaSH.Backend.Verilog.BoringTypes


-- | Clash Conponent converted to somethign approximating Verilog's module
component :: N.Component -> Component BB
component (N.Component name hports inpts outpt decls) =
  Component name (hwtype <$$> hports) (hwtype <$$> inpts) (hwtype <$> outpt) (declaration <$> decls)

type BB = (BB.BlackBoxTemplate, N.BlackBoxContext)

-- | Clash Decleration converted to somethign approximating Verilog's decleration
declaration :: N.Declaration -> Either BB (Declaration BB)
declaration = \case
  N.Assignment i e            -> Right $ Assignment [i] $ expr e
  N.CondAssignment i e branches -> Right $ CondAssignment i (expr e) $ custMap expr <$> branches
    where custMap f (x , y) = (f <$> x , f y)
  N.InstDecl i1 i2 portAssigns -> Right $ InstDecl i1 i2 $ expr <$$> portAssigns
  N.BlackBoxD tplt ctx         -> Left (tplt, ctx)
  N.NetDecl i t me             -> Right $ NetDecl i (hwtype t) $ expr <$> me


-- | Clash Hardware type converted to somethign approximating Verilog's fixed
-- types
hwtype :: N.HWType -> HWType
hwtype = \case
  N.Void         -> Bits undefined 0 -- try this
  N.Bool         -> Bits False 1

  N.Integer      -> Integer
  N.BitVector n  -> Bits False n
  N.Signed    n  -> Bits True  n

  -- TODO could be improoved ?
  N.Index     _  -> error $ $(curLoc) ++ "index types unsupported"

  N.Unsigned  n  -> Bits False n
  N.Vector  n t  -> Vector n $ hwtype t

  N.Sum n ctors  -> Sum n ctors
  N.Product n ts -> Product n $ hwtype <$> ts

  N.SP      n vs -> SP n $ hwtype <$$$> vs

  N.Clock _      -> hwtype N.Bool
  N.Reset _      -> hwtype N.Bool

expr :: N.Expr -> Expr BB
expr = \case
  N.DataCon _ _ _    -> error $ $(curLoc) ++ "Data constructors unsupported"
  N.DataTag _ _      -> error $ $(curLoc) ++ "Data tags unsupported"

  N.Literal mty l    -> literal (fst <$> mty) l
  N.Identifier i mm  -> modifier mm $ Right $ Identifier i
  N.BlackBoxE t c _ mm -> modifier mm $ Left (t, c)


modifier :: Maybe N.Modifier
         -> Either blackbox (BigRecur blackbox)
         -> Expr blackbox
modifier mm e = MTBBE $ MT Nothing $ case mm of
  Nothing                             -> E <$> e
  (Just (N.Indexed (ty, start, end))) -> Right $ Index start end $ MT (Just $ hwtype ty) e
  (Just (N.DC _))                     -> error $ $(curLoc) ++ "DataCon context unsupported"
  (Just (N.VecAppend))                -> error $ $(curLoc) ++ "Not sure how VecAppend works"

literal :: Maybe N.HWType -> N.Literal -> Expr blackbox
literal mty = \case
  N.NumLit n      -> simp $ Number n
  N.BoolLit False -> simp $ Number 0
  N.BoolLit True  -> simp $ Number 1
  N.BitLit N.H    -> literal mty $ N.BoolLit False
  N.BitLit N.L    -> literal mty $ N.BoolLit True
  N.BitLit N.U    -> simp $ Undefined
  N.BitLit N.Z    -> simp $ HighImpedence
  N.VecLit lits   -> MTBBE $ MT (hwtype <$> mty) $ Right $ E $ Concat $ case (lits, mty) of
    ([] , _)                    -> []
    (_  , Just (N.Vector _ ty)) -> literal (Just ty) <$> lits
    (_  , Nothing)              -> literal Nothing <$> lits -- this is probably dead code
    _                           -> error $ $(curLoc) ++ "Bad type for vector literal"

  where simp = MTBBE . MT (hwtype <$> mty) . Right . E . Literal

infixr 0 <$$>

(<$$>) :: (Functor fx, Functor fy) => (a -> b) -> fx (fy a) -> fx (fy b)
f <$$> x = (fmap . fmap) f x

(<$$$>) :: (Functor fx, Functor fy, Functor fz) => (a -> b) -> fx (fy (fz a)) -> fx (fy (fz b))
f <$$$> x = (fmap . fmap . fmap) f x
