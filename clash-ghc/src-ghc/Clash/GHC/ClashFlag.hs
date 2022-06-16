{-|
  Copyright   :  (C) 2022, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to parse and render flags exposed on the Clash's command line.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Clash.GHC.ClashFlag
  ( ClashFlag(..)
  , FlagName

  -- * Simple flag builders
  , boolFlag
  , wordFlag
  , intFlag
  , readableFlag

  , maybeStringFlag
  , maybeIntFlag
  , maybeWordFlag
  , maybeReadableFlag

  , maybeStringWithDefaultFlag

  -- * Generic flag builders
  , boolArg
  , unsetArg
  , wordSuffix, wordSuffixId
  , intSuffix, intSuffixId
  , strSuffix, strSuffixId

  -- * Flag renderers
  , renderNoArg
  , renderSuffix
  , renderSepSuffix

  -- * Utilities
  , deprecated
  , isDefault
  ) where

-- base
import Data.IORef (IORef, modifyIORef)

-- ghc
#if __GLASGOW_HASKELL__ >= 900
import GHC.Driver.CmdLine
#else
import CmdLineParser
#endif
  (EwM, Flag (Flag, flagOptKind), liftEwM, OptKind (..), defFlag, addErr, deprecate)

-- clash-lib
import Clash.Driver.Types (ClashOpts(..), defClashOpts)

-- lens
import Control.Lens (Lens', (^.), set)
import Text.Read (readMaybe)

-- | Defines a way to parse command line flags, set their values in a 'ClashOpts',
-- and render flags based on a 'ClashOpts'.
data ClashFlag = ClashFlag
  { cfFlags :: IORef ClashOpts -> [Flag IO]
  , cfRender :: ClashOpts -> [String]
  }

type FlagName = String

-- | Return two flags covering a single property: @-fclash-foo@ and @-fclash-no-foo@.
boolFlag :: FlagName -> Lens' ClashOpts Bool -> ClashFlag
boolFlag nm opt = ClashFlag
  { cfFlags = \opts -> boolArg nm opts (set opt)
  , cfRender = \opts -> if isDefault opt opts then [] else renderNoArg nm (opts ^. opt)
  }

-- | Return a flag setting a 'Word': @-fclash-foo=3@.
wordFlag :: FlagName -> Lens' ClashOpts Word -> ClashFlag
wordFlag nm opt = ClashFlag
  { cfFlags = \ref -> wordSuffixId nm ref (set opt)
  , cfRender = \opts ->
      if isDefault opt opts then [] else renderSuffix nm (show (opts ^. opt))
  }

-- | Return a flag setting a 'Int': @-fclash-foo=3@.
intFlag :: FlagName -> Lens' ClashOpts Int -> ClashFlag
intFlag nm opt = ClashFlag
  { cfFlags = \ref -> intSuffixId nm ref (set opt)
  , cfRender = \opts ->
      if isDefault opt opts then [] else renderSuffix nm (show (opts ^. opt))
  }

-- | Return two flags setting a 'Maybe' 'String': @-fclash-foo wibble@ and
-- @-fclash-no-foo@.
maybeStringFlag :: FlagName -> Lens' ClashOpts (Maybe String) -> ClashFlag
maybeStringFlag nm opt = ClashFlag
  { cfFlags = \opts ->
         strSuffixId nm opts (set opt . Just)
      ++ unsetArg nm opts (set opt)
  , cfRender = \opts ->
      if isDefault opt opts
      then []
      else case opts ^. opt of
        Nothing -> renderNoArg nm False
        Just s -> renderSepSuffix nm s
  }


-- | Return three flags setting a 'Maybe' 'String'. If no argument is supplied, a
-- default value is set. For example, if the default is @simpleDefault@, then:
--
--   * @-fclash-foo@ sets the option to @Just "simpleDefault"@
--   * @-fclash-foo=wibble@ sets the option to @Just "wibble"@
--   * @-fclash-no-foo@ sets the option to @Nothing@
--
maybeStringWithDefaultFlag ::
  FlagName ->
  -- | Default
  String ->
  Lens' ClashOpts (Maybe String) ->
  ClashFlag
maybeStringWithDefaultFlag flagName dflt opt = ClashFlag
  { cfFlags = \opts ->
         anySuffix flagName opts parser (set opt)
      ++ unsetArg flagName opts (set opt)
  , cfRender = \opts ->
      if isDefault opt opts
      then []
      else case opts ^. opt of
        Nothing -> renderNoArg flagName False
        Just s | s == dflt -> renderSuffix flagName ""
        Just s -> renderSuffix flagName s
  }
 where
  parser :: String -> EwM IO (Maybe (Maybe String))
  parser ['='] = pure (Just (Just dflt))
  parser ('=':v) = pure (Just (Just v))
  parser s = do
    addErr ("Could not parse: " <> s)
    pure Nothing

-- | Return two flags setting a 'Maybe' 'Int'.
--
--   * @-fclash-foo=3@ sets the option to @Just 3@
--   * @-fclash-no-foo@ sets the option to @Nothing@
--
maybeIntFlag :: FlagName -> Lens' ClashOpts (Maybe Int) -> ClashFlag
maybeIntFlag nm opt = ClashFlag
  { cfFlags = \ref ->
         intSuffixId nm ref (set opt . Just)
      ++ unsetArg nm ref (set opt)

  , cfRender = \opts ->
      if isDefault opt opts
      then []
      else case opts ^. opt of
        Nothing -> renderNoArg nm False
        Just a -> renderSuffix nm (show a)
  }

-- | Return two flags setting a 'Maybe' 'Word'.
--
--   * @-fclash-foo=3@ sets the option to @Just 3@
--   * @-fclash-no-foo@ sets the option to @Nothing@
--
maybeWordFlag :: FlagName -> Lens' ClashOpts (Maybe Word) -> ClashFlag
maybeWordFlag nm opt = ClashFlag
  { cfFlags = \ref ->
         wordSuffixId nm ref (set opt . Just)
      ++ unsetArg nm ref (set opt)

  , cfRender = \opts ->
      if isDefault opt opts
      then []
      else case opts ^. opt of
        Nothing -> renderNoArg nm False
        Just a -> renderSuffix nm (show a)
  }

maybeReadableFlag ::
  forall a.
  (Eq a, Read a, Show a) =>
  FlagName ->
  Lens' ClashOpts (Maybe a) ->
  ClashFlag
maybeReadableFlag nm opt = ClashFlag
  { cfFlags = \ref ->
         strSuffix nm ref parser (set opt)
      ++ unsetArg nm ref (set opt)

  , cfRender = \opts ->
      if isDefault opt opts
      then []
      else case opts ^. opt of
        Nothing -> renderNoArg nm False
        Just a -> renderSepSuffix nm (show a)
  }
 where
  parser :: String -> EwM IO (Maybe (Maybe a))
  parser s = case readMaybe s of
    Nothing -> do
      addErr ("Could not parse flag argument: " <> s)
      pure Nothing
    a -> pure (Just a)


readableFlag ::
  forall a.
  (Eq a, Read a, Show a) =>
  FlagName ->
  Lens' ClashOpts a ->
  ClashFlag
readableFlag nm opt = ClashFlag
  { cfFlags = \ref -> strSuffix nm ref parser (set opt)
  , cfRender = \opts ->
      if isDefault opt opts then [] else renderSepSuffix nm (show (opts ^. opt))
  }
 where
  parser :: String -> EwM IO (Maybe a)
  parser s = case readMaybe s of
    Nothing -> addErr ("Could not parse " <> s)  >> pure Nothing
    a -> pure a

-- | Helper function....
validateAndSet ::
  IORef ClashOpts ->
  (a -> EwM IO (Maybe b)) ->
  (b -> ClashOpts -> ClashOpts) ->
  a ->
  EwM IO ()
validateAndSet ref validateFunc setFunc v0 = do
  v1 <- validateFunc v0
  case v1 of
    Nothing -> pure ()
    Just v2 -> liftEwM (modifyIORef ref (setFunc v2))

-- | Build NoArg flag....
boolArg ::
  FlagName ->
  IORef ClashOpts ->
  (Bool -> ClashOpts -> ClashOpts) ->
  [Flag IO]
boolArg flagName ref setFunc =
  [ defFlag ("fclash-"    <> flagName) (NoArg (wrappedSetFunc True))
  , defFlag ("fclash-no-" <> flagName) (NoArg (wrappedSetFunc False)) ]
 where
  wrappedSetFunc v = liftEwM (modifyIORef ref (setFunc v))

unsetArg ::
  String ->
  IORef ClashOpts ->
  (Maybe a -> ClashOpts -> ClashOpts) ->
  [Flag IO]
unsetArg flagName ref setFunc =
  [defFlag ("fclash-no-" <> flagName) (NoArg (unsetEwM ref))]
 where
  unsetEwM opts = liftEwM (modifyIORef opts (setFunc Nothing))

-- | Build IntSuffix / WordSuffix flag....
wordSuffix ::
  FlagName ->
  IORef ClashOpts ->
  -- | Parser / validator
  (Word -> EwM IO (Maybe b)) ->
  -- | Setter
  (b -> ClashOpts -> ClashOpts) ->
  [Flag IO]
wordSuffix flagName ref validateFunc setFunc =
  -- TODO: Newer versions of GHC support 'WordSuffix'. Use with CPP.
  -- TODO: Validate for older versions of GHC
  [ defFlag
      ("fclash-" <> flagName)
      (IntSuffix (validateAndSet ref validateFunc setFunc . fromIntegral)) ]

-- | Build IntSuffix / WordSuffix flag....
wordSuffixId ::
  FlagName ->
  IORef ClashOpts ->
  -- | Setter
  (Word -> ClashOpts -> ClashOpts) ->
  [Flag IO]
wordSuffixId flagName ref = wordSuffix flagName ref (pure . Just)

-- | Build IntSuffix flag....
intSuffix ::
  FlagName ->
  IORef ClashOpts ->
  -- | Parser / validator
  (Int -> EwM IO (Maybe b)) ->
  -- | Setter
  (b -> ClashOpts -> ClashOpts) ->
  [Flag IO]
intSuffix flagName ref validateFunc setFunc =
  [ defFlag
      ("fclash-" <> flagName)
      (IntSuffix (validateAndSet ref validateFunc setFunc)) ]

-- | Build IntSuffix flag....
intSuffixId ::
  FlagName ->
  IORef ClashOpts ->
  -- | Setter
  (Int -> ClashOpts -> ClashOpts) ->
  [Flag IO]
intSuffixId flagName ref = intSuffix flagName ref (pure . Just)

-- | Build StrSuffix flag....
strSuffix ::
  FlagName ->
  IORef ClashOpts ->
  -- | Parser / validator
  (String -> EwM IO (Maybe b)) ->
  -- | Setter
  (b -> ClashOpts -> ClashOpts) ->
  [Flag IO]
strSuffix flagName ref validateFunc setFunc =
  [ defFlag
      ("fclash-" <> flagName)
      (SepArg (validateAndSet ref validateFunc setFunc)) ]

-- | Build IntSuffix flag....
strSuffixId ::
  FlagName ->
  IORef ClashOpts ->
  -- | Setter
  (String -> ClashOpts -> ClashOpts) ->
  [Flag IO]
strSuffixId flagName ref = strSuffix flagName ref (pure . Just)


anySuffix ::
  forall b.
  FlagName ->
  IORef ClashOpts ->
  -- | Parser / validator
  (String -> EwM IO (Maybe b)) ->
  -- | Setter
  (b -> ClashOpts -> ClashOpts) ->
  [Flag IO]
anySuffix flagName ref parser setter =
  [ defFlag
      ("fclash-" <> flagName)
      (AnySuffix (validateAndSet ref validateFunc setter)) ]
 where
  validateFunc :: String -> EwM IO (Maybe b)
  validateFunc = parser . drop (length ("-fclash-" <> flagName))


-- | Render a bool flag with given name and value unconditionally
renderNoArg :: FlagName -> Bool -> [String]
renderNoArg flagName value
  | value = ["-fclash-" <> flagName]
  | otherwise = ["-fclash-no-" <> flagName]

-- | Render a flag @-fclash-foo arg@. This is the default flag type for
-- everything that's /not/ a 'Word' or 'Int'.
renderSepSuffix :: FlagName -> String -> [String]
renderSepSuffix flagName value = ["-fclash-" <> flagName, value]

-- | Render a flag @-fclash-foo=arg@. This is the default flag type for
-- 'Word' and 'Int'.
renderSuffix :: FlagName -> String -> [String]
renderSuffix flagName value = ["-fclash-" <> flagName <> "=" <> value]

-- | Add a deprecation warning to a 'ClashFlag'
deprecated :: String -> ClashFlag -> ClashFlag
deprecated msg cf = cf{cfFlags = map go . cfFlags cf}
 where
  go :: Flag IO -> Flag IO
  go flag@Flag{flagOptKind} = flag{flagOptKind = goOpKind flagOptKind}

  goOpKind :: OptKind IO -> OptKind IO
  goOpKind = \case
    NoArg em -> NoArg (deprecate msg >> em)
    HasArg f -> goFunc HasArg f
    SepArg f -> goFunc SepArg f
    Prefix f -> goFunc Prefix f
    OptPrefix f -> goFunc OptPrefix f
    OptIntSuffix f -> goFunc OptIntSuffix f
    IntSuffix f -> goFunc IntSuffix f
    FloatSuffix f -> goFunc FloatSuffix f
    PassFlag f -> goFunc PassFlag f
    AnySuffix f -> goFunc AnySuffix f
#if __GLASGOW_HASKELL__ <= 806
    PrefixPred _ _ -> error "goOpKind: unexpected constructor PrefixPred"
    AnySuffixPred _ _ -> error "goOpKind: unexpected constructor AnySuffixPred"
#endif

  goFunc :: ((a -> EwM IO ()) -> OptKind IO) -> (a -> EwM IO ()) -> OptKind IO
  goFunc constr f = constr (\a -> deprecate msg >> f a)

-- | Given a way to retrieve a value from a 'ClashOpts', determine whether the
-- value in the given 'ClashOpts' corresponds to the one in 'defClashOpts'.
isDefault :: Eq a => Lens' ClashOpts a -> ClashOpts -> Bool
isDefault opt opts = defClashOpts ^. opt == opts ^. opt
