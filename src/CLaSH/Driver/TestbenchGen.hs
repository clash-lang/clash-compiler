{-# LANGUAGE ViewPatterns #-}
module CLaSH.Driver.TestbenchGen where

import           Control.Error               (EitherT,eitherT,hoistEither,left,note,right)
import           Control.Monad.Trans.Class   (lift)
import           Data.Either                 (lefts)
import           Data.HashMap.Lazy           (HashMap)
import qualified Data.HashMap.Lazy           as HashMap
import           Unbound.LocallyNameless     (bind,embed,makeName,name2String,name2Integer,rec,unrec)
import           Unbound.LocallyNameless.Ops (unsafeUnbind)

import CLaSH.Core.DataCon
import CLaSH.Core.Pretty
import CLaSH.Core.Term
import CLaSH.Core.TyCon
import CLaSH.Core.Type
import CLaSH.Core.Util
import CLaSH.Core.Var

import CLaSH.Netlist
import CLaSH.Netlist.Types
import CLaSH.Primitives.Types

import CLaSH.Util

-- genTestBench ::
--   HashMap TmName (Type,Term)
--   -> Maybe TmName
--   -> Maybe TmName
--   -> Component

genTestBench vhdlState primMap globals normalizeSignal stimuliNmM = eitherT error return $ do
  inpDecls <- (flip (maybe (right []))) stimuliNmM $ \stimuliNm -> do
    (decls,sigVs,comps,vhdlState') <- prepareSignals vhdlState primMap globals normalizeSignal stimuliNm
    undefined

  undefined

prepareSignals ::
  VHDLState
  -> PrimMap
  -> HashMap TmName (Type,Term)
  -> (HashMap TmName (Type,Term) -> TmName -> [(TmName,(Type,Term))])
  -> TmName
  -> EitherT String IO ([Declaration],[Id],[Component],VHDLState)
prepareSignals vhdlState primMap globals normalizeSignal stimuliNm = do
   (stimuliTy,stimuliTm) <- hoistEither $ note ($(curLoc) ++ "Unable to find: " ++ name2String stimuliNm) (HashMap.lookup stimuliNm globals)
   stimuliList           <- termToList stimuliTm
   elemTy                <- stimuliElemTy stimuliTy
   let elemNms  = map (\i -> makeName (name2String stimuliNm ++ show i) (name2Integer stimuliNm)) [(0::Int)..]
       elemBnds = zipWith (\nm e -> (nm,(elemTy,e))) elemNms stimuliList
       stimuliList_normalized = map (normalizeSignal (HashMap.fromList elemBnds `HashMap.union` globals)) (map fst elemBnds)
   lift $ createSignal vhdlState primMap stimuliList_normalized


termToList :: Monad m => Term -> EitherT String m [Term]
termToList e = case (second lefts $ collectArgs e) of
  (Data _ dc,[])
    | name2String (dcName dc) == "GHC.Types.[]" -> pure []
    | otherwise                                 -> left $ $(curLoc) ++ "Can't deconstruct list literal: " ++ showDoc e
  (Data _ dc,[hdArg,tlArg])
    | name2String (dcName dc) == "GHC.Types.:"  -> (hdArg:) <$> termToList tlArg
    | otherwise                                 -> left $ $(curLoc) ++ "Can't deconstruct list literal: " ++ showDoc e
  _ -> left $ $(curLoc) ++ "Can't deconstruct list literal: " ++ showDoc e

stimuliElemTy ::Monad m => Type -> EitherT String m Type
stimuliElemTy ty = case splitTyConAppM ty of
  (Just (tc,[arg]))
    | name2String (tyConName tc) == "GHC.Types.[]" -> return arg
    | otherwise -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty
  _ -> left $ $(curLoc) ++ "Not a List TyCon: " ++ showDoc ty

createSignal ::
  VHDLState
  -> PrimMap
  -> [[(TmName,(Type,Term))]]
  -> IO ([Declaration],[Id],[Component],VHDLState)
createSignal vhdlState primMap normalizedSignals = do
  let (signalHds,signalTls) = unzip $ map (\(l:ls) -> (l,ls)) normalizedSignals
      (sigVs,sigEs)         = unzip $ map (\(_,(_,Letrec (unsafeUnbind -> (bndrs,(Var topTy topNm))))) -> (Id topNm (embed topTy),unrec bndrs)) signalHds
      newExpr               = Letrec $ bind (rec $ concat sigEs) (Var (fst . snd $ head signalHds) (fst $ head signalHds))
      newBndr               = ((fst $ head signalHds), (fst . snd $ head signalHds, newExpr))

  ((Component _ _ _ _ decls):comps,vhdlState') <- genNetlist (Just vhdlState) (HashMap.fromList $ newBndr : concat signalTls) primMap (fst $ head signalHds)
  return (decls,sigVs,comps,vhdlState')
