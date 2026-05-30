module Metadata (validateMetadata) where

import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as L
import Data.Set qualified as Set
import Hakyll
import Himari

-- | typoを検出するためメタデータの属性が許可されたものだけであるとバリデーションします。
validateMetadata :: Identifier -> Compiler ()
validateMetadata identifier = do
  metadata <- getMetadata identifier
  let unknownKeys =
        filter (`Set.notMember` allowedMetadataKeys) $
          Key.toString <$> KeyMap.keys metadata
  case unknownKeys of
    [] -> return ()
    keys ->
      fail $
        "Unknown metadata keys in " <> toFilePath identifier <> ": " <> L.intercalate ", " keys

-- | 許可されているメタデータ属性。
-- `fail`や`toFilePath`が`String`を扱うため`Text`より`String`の方が都合が良い。
-- これぐらいの数なら許容します。
allowedMetadataKeys :: Set String
allowedMetadataKeys = Set.fromList ["title", "date", "updated"]
