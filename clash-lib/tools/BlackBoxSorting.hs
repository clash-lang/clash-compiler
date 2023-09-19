#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as Aeson
import Data.ByteString.Lazy.Search (replace)
import Data.String (IsString)
#endif

{- NOTE [Sorting YAML object keys]

'Yaml.encode' encodes object with their keys in alphabetical order.
For readability we like `name` to be at the top, and `type` to be just above `template`.

We accomplice this here by renaming those keys to something there sorts where
we like them to be. And find-and-replace those temporary names back
in the resulting ByteString.
-}
#if MIN_VERSION_aeson(2,0,0)
keySortingRenames :: IsString str => [(str,str)]
keySortingRenames =
  [ ("name", "aaaa_really_should_be_name_but_renamed_to_get_the_sorting_we_like")
  , ("type", "really_should_be_type_but_renamed_to_get_the_sorting_we_like")
  ]

customSortOutput :: Aeson.Value -> Aeson.Value
customSortOutput x = case x of
  Aeson.Object o -> Aeson.Object $ fmap customSortOutput $ renameKeys $ o
  Aeson.Array xs -> Aeson.Array $ fmap customSortOutput xs
  _ -> x
 where
  renameKeys obj = foldl renameKey obj keySortingRenames
  renameKey obj (kOld,kNew) =
    case Aeson.lookup kOld obj of
      Nothing -> obj
      Just val -> Aeson.insert kNew val (Aeson.delete kOld obj)

removeTempKey :: BS.ByteString -> BS.ByteString
removeTempKey inp = BL.toStrict $ foldl go (BL.fromStrict inp) keySortingRenames
 where
  go bs (orig,temp) = replace temp orig bs

customYamlEncode :: Aeson.Value -> BS.ByteString
customYamlEncode = removeTempKey . Yaml.encode . customSortOutput
#else
-- < aeson-2.0 stores keys in HashMaps, whose order we can't possibly predict.
customYamlEncode :: Aeson.Value -> BS.ByteString
customYamlEncode = Yaml.encode
#endif
