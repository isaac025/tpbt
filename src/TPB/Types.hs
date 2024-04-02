{-# LANGUAGE DeriveAnyClass #-}

module TPB.Types where

import Control.Monad (mzero)
import Control.Monad.Catch
import Data.Aeson (FromJSON (..), Value (Array), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Vector (toList)
import GHC.Generics (Generic)
import Network.HTTP.Simple (JSONException)
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
    { added :: String
    , category :: String
    , id :: String
    , imdb :: String
    , infoHash :: String
    , leechers :: String
    , name :: String
    , numFiles :: String
    , seeders :: String
    , size :: String
    , status :: String
    , username :: String
    }
    deriving (Generic, Show)

instance FromJSON Result where
    parseJSON = genericParseJSON (defaultOptions{fieldLabelModifier = camelTo2 '_'})

data Results = Results [Result]

instance FromJSON Results where
    parseJSON (Array v) = Results <$> traverse parseJSON (toList v)
    parseJSON _ = mzero

data TPBError
    = RequestError String
    | JSONFormat JSONException
    | NetworkError SomeException
    deriving (Exception)

instance Show TPBError where
    show (RequestError s) = show s
    show (JSONFormat e) = "Error parsing json: " ++ show e
    show (NetworkError _) = "Network communication error"
