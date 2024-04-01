{-# LANGUAGE DeriveAnyClass #-}

module TPB.Types where

import Control.Monad.Catch
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Category
    = All
    | Audio
    | Video
    | Applications
    | Games
    | Other
    deriving (Show, Bounded, Enum)

class CatNum a where
    toCatNum :: a -> String

instance CatNum Category where
    toCatNum All = "0"
    toCatNum Audio = "100"
    toCatNum Video = "200"
    toCatNum Applications = "300"
    toCatNum Games = "400"
    toCatNum Other = "600"

data SearchOptions = SearchOptions
    { searchField :: String
    , searchCategory :: Category
    }

data Result = Result
    { id :: String
    , name :: String
    , infoHash :: String
    , leechers :: String
    , seeders :: String
    , numFiles :: String
    , size :: String
    , username :: String
    , added :: String
    , status :: String
    , category :: String
    , imdb :: String
    }
    deriving (Generic, FromJSON, Show)

data TPBError
    = RequestError String
    | JSONFormat
    | NetworkError SomeException
    deriving (Exception)

instance Show TPBError where
    show (RequestError s) = show s
    show JSONFormat = "Error parsing json"
    show (NetworkError _) = "Network communication error"
