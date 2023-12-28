module Main where

import Codec.Binary.UTF8.String (utf8Encode)
import Data.Aeson hiding (encode)
import Data.Foldable (traverse_)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.HTTP.Simple
import Network.URI.Encode (encode)
import Prelude hiding (id)

data Song = Song
    { id :: String
    , name :: String
    , info_hash :: String
    , leechers :: String
    , seeders :: String
    , num_files :: String
    , size :: String
    , username :: String
    , added :: String
    , status :: String
    , category :: String
    , imdb :: String
    }
    deriving (Show, Generic)

instance ToJSON Song
instance FromJSON Song

-- TPB api link
api :: String
api = "https://apibay.org/"

-- function to make our torrrent link
torrentLink :: Song -> String
torrentLink Song{name, info_hash} = "magnet:?xt=urn:btih:" ++ info_hash ++ "&dn=" ++ name' ++ "&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A6969%2Fannounce&tr=udp%3A%2F%2Fopen.stealth.si%3A80%2Fannounce&tr=udp%3A%2F%2Ftracker.torrent.eu.org%3A451%2Fannounce&tr=udp%3A%2F%2Ftracker.bittor.pw%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337&tr=udp%3A%2F%2Fpublic.popcorn-tracker.org%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.dler.org%3A6969%2Fannounce&tr=udp%3A%2F%2Fexodus.desync.com%3A6969&tr=udp%3A%2F%2Fopentracker.i2p.rocks%3A6969%2Fannounce"
  where
    name' = encodeTitle name

-- encode song/audio title to URI utf8
encodeTitle :: String -> String
encodeTitle = encode . utf8Encode

-- Search on API
search :: String -> String
search (words -> s) = "q.php?q=" <> s' <> "&cat=101"
  where
    s' = intercalate "%20" s

main :: IO ()
main = do
    l <- getLine
    req <- parseRequest ("GET " <> api <> search l)
    resp <- httpJSON req :: IO (Response [Song])
    let ls = getResponseBody resp
    traverse_ (\x -> putStrLn (name x ++ ": " ++ torrentLink x)) ls
