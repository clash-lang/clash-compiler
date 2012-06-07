module CLaSH.GHC.LoadInterfaceFiles where

-- External Modules
import           Data.Maybe (mapMaybe)

-- GHC API
import qualified Exception
import qualified GHC
import qualified HscTypes
import qualified IfaceSyn
import qualified LoadIface
import qualified Maybes
import qualified MonadUtils
import           Outputable (showPpr,text)
import qualified TcIface
import qualified TcRnMonad
import qualified TcRnTypes
import qualified UniqFM

-- Internal Modules
import           CLaSH.Util (traceIf,mapAccumLM)

getExternalTyCons ::
  GHC.GhcMonad m
  => [GHC.ModuleName]
  -> GHC.ModuleName
  -> m ([GHC.ModuleName],[GHC.TyCon])
getExternalTyCons visited modName = (`Exception.gcatch` expCatch) $ do
  foundMod       <- GHC.findModule modName Nothing
  (tcs,used) <- runIfl foundMod $ do
            ifaceM <- loadIface foundMod
            case ifaceM of
              Nothing -> return ([],[])
              Just iface -> do
                let used  = map fst $ HscTypes.dep_mods $ GHC.mi_deps iface
                let decls = map snd (GHC.mi_decls iface)
                tcs <- fmap (mapMaybe tyThingIsTyCon) $ mapM loadDecl decls
                return (tcs,used)

  let visited' = modName:visited
  let used'    = filter (`notElem` visited') used
  (visited'',tcs') <- mapAccumLM getExternalTyCons (visited' ++ used')
                       used'
  return (visited'',tcs ++ concat tcs')
  where
    expCatch :: GHC.GhcMonad m
      => HscTypes.SourceError -> m ([GHC.ModuleName],[GHC.TyCon])
    expCatch _ = return (modName:visited,[])

    tyThingIsTyCon :: GHC.TyThing -> Maybe GHC.TyCon
    tyThingIsTyCon (GHC.ATyCon tc) = Just tc
    tyThingIsTyCon _               = Nothing

runIfl :: GHC.GhcMonad m => GHC.Module -> TcRnTypes.IfL a -> m a
runIfl modName action = do
  hscEnv <- GHC.getSession
  let localEnv = TcRnTypes.IfLclEnv modName (text "runIfl")
                   UniqFM.emptyUFM UniqFM.emptyUFM
  let globalEnv = TcRnTypes.IfGblEnv Nothing
  MonadUtils.liftIO $ TcRnMonad.initTcRnIf 'r' hscEnv globalEnv
                        localEnv action

loadDecl :: IfaceSyn.IfaceDecl -> TcRnTypes.IfL GHC.TyThing
loadDecl decl = TcIface.tcIfaceDecl False decl

loadIface :: GHC.Module -> TcRnTypes.IfL (Maybe GHC.ModIface)
loadIface foundMod = do
  ifaceFailM <- LoadIface.findAndReadIface (Outputable.text "loadIface") foundMod False
  case ifaceFailM of
    Maybes.Succeeded (modInfo,_) -> return (Just modInfo)
    Maybes.Failed _ -> traceIf True ("failed to load interface for module: " ++ showPpr foundMod) $ return Nothing
