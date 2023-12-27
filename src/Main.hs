module Main where

import Data.Aeson
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.HTTP.Simple
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
    deriving (Show, Generic, FromJSON, ToJSON)

api :: String
api = "https://apibay.org/"

search :: String -> String
search (words -> s) = "q.php?q=" <> s' <> "&cat=101"
  where
    s' = intercalate "%20" s

main :: IO ()
main = do
    l <- getLine
    req <- parseRequest ("GET " <> api <> search l)
    resp <- httpBS req
    print (getResponseBody resp)
