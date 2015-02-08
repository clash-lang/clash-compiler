{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Generate VHDL for assorted Netlist datatypes
module CLaSH.Backend.Verilog (VerilogState) where

import           Control.Applicative
import           Control.Lens hiding (Indexed)
-- import           Control.Monad                        (forM,join,liftM,when,zipWithM)
-- import           Control.Monad.State                  (State)
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
-- import           CLaSH.Netlist.Types
-- import           CLaSH.Netlist.Util
import           CLaSH.Util                           (curLoc)

import           CLaSH.Backend.Verilog.BoringTypes as B
import           CLaSH.Backend.Verilog.Bore

-- | State for the 'CLaSH.Netlist.VHDL.VHDLM' monad:
data VerilogState = VerilogState {}

makeLenses ''VerilogState

instance Backend VerilogState where
  init            = VerilogState
  extractTypes    = const HashSet.empty
  name            = const "verilog"
  extension       = const ".v"

  genHDL          = return . ("main",) . pp . toModule . component
  mkTyPackage     = const $ return D.empty
  hdlType         = return . D.text . T.pack . show . toVTy . hwtype
  hdlTypeErrValue = error $ $(curLoc) ++ "not yet implemented"
  hdlTypeMark     = error $ $(curLoc) ++ "not yet implemented"
  inst            = return . Just . pp . toItemBB . declaration
  expr _ e        = return $ pp $ toVExpr $ CLaSH.Backend.Verilog.Bore.expr e

-- TODO replace orphan instances with implicit arguments?

instance PrettyPrint Text where
  pp = D.text

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pp = \case
    Left a -> pp a
    Right b -> pp b

newtype VExpr iden blackbox = VE (Either blackbox (V.Expr iden (VExpr iden blackbox)))
                            deriving (Show, PrettyPrint)


-- TODO: exploit the types to make this more flexible
-- TODO: state monad to bind index'd expressions and index gensym id
toVExpr :: B.Expr blackbox -> VExpr Text blackbox
toVExpr (MTBBE (MT _ (Left bb))) = VE $ Left bb
toVExpr (MTBBE (MT t (Right e))) = VE $ Right $ case e of
  E (B.Literal lit)  -> V.Literal (toVTy <$> t) lit
  E (B.Concat es)    -> V.LValue $ V.Concat $ toVExpr <$> es
  E (B.Identifier i) -> V.LValue $ V.Identifier i
  B.Index h l _      -> V.LValue $ V.Range
                        (error $ $(curLoc) ++ "not yet implemented")
                        (nToVE $ fromIntegral h, nToVE $ fromIntegral l)

toVTy :: B.HWType -> Int
toVTy = \case
  B.Integer -> error $ $(curLoc) ++ "integers are not synthesizable"
  B.Bits lst -> sum $ fst <$> lst

toItem :: forall blackbox. Declaration blackbox -> ModuleItem Text (VExpr Text blackbox)
toItem = \case
  Assignment is e -> V.Assign (V.Concat $ idToVE <$> is) $ toVExpr e

  CondAssignment i sc es -> Assign (V.Identifier i) $ conds es
    where
      conds :: [(Maybe (B.Expr blackbox), B.Expr blackbox)] -> (VExpr Text blackbox)
      conds = \case
        []                -> error $ $(curLoc) ++ "CondAssigment cannot have zero branches"
        [(_,e)]           -> toVExpr e
        ((Nothing,e):_)   -> toVExpr e
        ((Just c ,e):es') -> injE $ V.Mux (injE $ BinOp Eq (toVExpr c) (toVExpr sc)) (toVExpr e) (conds es')

  InstDecl m i  ps -> V.Instance m [] i $ Just . toVExpr <$$> ps

  NetDecl i t me -> Wire (Just (nToVE $ fromIntegral $ toVTy t, nToVE 0)) [(i, toVExpr <$> me)]

  where injE = VE . Right
        idToVE e = injE $ LValue $ V.Identifier $ e


nToVE :: Integer -> VExpr iden blackbox
nToVE  n = VE $ Right $ V.Literal Nothing $ V.Number n

type VItem blackbox = Either blackbox (ModuleItem Text (VExpr Text blackbox))

-- TODO use some lens mapLeft shit
toItemBB :: Either blackbox (Declaration blackbox) -> VItem blackbox
toItemBB = \case
  Left bb -> Left bb
  Right x -> Right $ toItem x

toModule :: forall blackbox. Component blackbox -> V.Module Text (VItem blackbox)
toModule (Component modName ports ins out decls) = Module modName (fst <$> out : ins) items
  where items :: [VItem blackbox]
        items = [f Output out]
                ++ (f Input <$> ins)
                ++ (f wire <$> ports)
                ++ (toItemBB <$> decls)

        f ctor (i, t) = Right $ ctor range [i]
          where range = case toVTy t of
                  0 -> Nothing
                  n -> Just (nToVE $ fromIntegral n, nToVE 0)

        wire r ps = Wire r $ (,Nothing) <$> ps
