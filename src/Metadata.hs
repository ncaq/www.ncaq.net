module Metadata (validateMetadata) where

import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import Hakyll

-- | typoを検出するためメタデータの属性が許可されたものだけであるとバリデーションします。
validateMetadata :: Identifier -> Compiler ()
validateMetadata identifier = do
  metadata <- getMetadata identifier
  let unknownKeys = filter (`Set.notMember` allowedMetadataKeys) $ AK.toString <$> KeyMap.keys metadata
  case unknownKeys of
    [] -> return ()
    keys -> fail $ "Unknown metadata keys in " <> toFilePath identifier <> ": " <> L.intercalate ", " keys

-- | 許可されているメタデータ属性。
-- `fail`や`toFilePath`が`String`を扱うため`Text`より`String`の方が都合が良い。
-- これぐらいの数なら許容します。
allowedMetadataKeys :: Set String
allowedMetadataKeys = Set.fromList ["title", "date", "updated"]
