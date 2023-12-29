module TPB.Internals where

import Codec.Binary.UTF8.String (utf8Encode)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.URI.Encode (encode)
import Prelude hiding (id)

api :: String
api = "https://apibay.org/"

encodeTitle :: String -> String
encodeTitle = encode . utf8Encode

mkURL :: String -> String
mkURL (words -> s) = "q.php?q=" <> s' <> "&cat=101"
  where
    s' = intercalate "%20" s
