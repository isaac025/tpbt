{-# LANGUAGE DeriveAnyClass #-}

module TPB.Types where

import Control.Monad (mzero)
import Control.Monad.Catch
import Data.Aeson (FromJSON (..), Value (Array), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Vector (Vector, toList)
import GHC.Generics (Generic)
import Network.HTTP.Simple (JSONException)
import Prelude hiding (id)

-- | Categories for searching on the pirate bay api
data Category
    = All
    | Audio
    | Video
    | Applications
    | Games
    | Other
    deriving (Show, Bounded, Enum)

-- | Class to pass the actual argument to the api
class CatNum a where
    toCatNum :: a -> String

instance CatNum Category where
    toCatNum All = "0"
    toCatNum Audio = "100"
    toCatNum Video = "200"
    toCatNum Applications = "300"
    toCatNum Games = "400"
    toCatNum Other = "600"

-- | Results received from the API as json
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
    deriving (Generic)

instance Show Result where
    show Result{category, name, added, size} = unwords [show category, name, added, size]
instance FromJSON Result where
    parseJSON = genericParseJSON (defaultOptions{fieldLabelModifier = camelTo2 '_'})

-- | newtype wrapper of results since the api returns an JSON Array
newtype Results = Results [Result]

instance FromJSON Results where
    parseJSON (Array v) = Results <$> traverse parseJSON (toList v)
    parseJSON _ = mzero

-- | The content of an individual result
data Content = Content
    { cname :: Vector String
    , csize :: Vector Int
    }
    deriving (Generic, Show)

instance FromJSON Content where
    parseJSON = genericParseJSON (defaultOptions{fieldLabelModifier = tail})

newtype Contents = Contents [Content]

instance FromJSON Contents where
    parseJSON (Array v) = Contents <$> traverse parseJSON (toList v)
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
