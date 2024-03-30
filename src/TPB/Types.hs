{-# LANGUAGE DeriveAnyClass #-}

module TPB.Types where

import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Prelude hiding (id)

{- Search Types -}

-- | SearchField - a string to search
newtype SearchField = SearchField String

-- | Category - type of content to search
data Category
    = All
    | Audio
    | Video
    | Applications
    | Games
    | Other
    deriving (Show, Bounded, Enum)

-- | Class to convert a Category to ByteString
class ToByteString a where
    toByteString :: a -> ByteString

instance ToByteString Category where
    toByteString All = ""
    toByteString Audio = "100"
    toByteString Video = "100"
    toByteString Applications = "100"
    toByteString Games = "100"
    toByteString Other = "100"

-- | Options to pass to request
data SearchOptions = SearchOptions
    { searchField :: SearchField
    , searchCategory :: Category
    }

{- Result Types -}

-- | Id of result
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
