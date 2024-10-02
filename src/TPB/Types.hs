{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module TPB.Types where

import Data.Aeson (FromJSON (..), Value (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON, withObject, (.:))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Prelude hiding (id)

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
    deriving (Generic, Typeable)

instance Show Result where
    show Result{..} = cleanName <> " " <> bytesToMegaBytes size <> "Mb"
      where
        bytesToMegaBytes :: String -> String
        bytesToMegaBytes (read -> size' :: Int) =
            let bytes = show $ fromIntegral size' / 1e6
             in roundToTwo bytes

        roundToTwo :: String -> String
        roundToTwo x = takeWhile (/= '.') x <> take 3 (dropWhile (/= '.') x)

        cleanName :: String
        cleanName = [x | x <- name, x `elem` ['a' .. 'z'] || x `elem` ['A' .. 'Z'] || x `elem` [' ', '-']]

instance FromJSON Result where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

data Song = Song Value Value
    deriving (Generic, Typeable)

instance Show Song where
    show (Song n s) = show n <> " " <> show s

instance FromJSON Song where
    parseJSON = withObject "Song" $ \v ->
        Song
            <$> v .: "name"
            <*> v .: "size"
data Audio
    = Music
    | AudioBooks
    | SoundClips
    | FLAC
    | Other
    deriving (Enum)

class ToCat a where
    toCat :: a -> String

instance ToCat Audio where
    toCat Music = "101"
    toCat AudioBooks = "102"
    toCat SoundClips = "103"
    toCat FLAC = "104"
    toCat Other = "199"

instance Show Audio where
    show Music = "Music"
    show AudioBooks = "Audio Books"
    show SoundClips = "Sound Clips"
    show FLAC = "FLAC"
    show Other = "Other"

instance Read Audio where
    readsPrec _ value =
        case value of
            "Music" -> [(Music, "")]
            "Audio Books" -> [(AudioBooks, "")]
            "Sound Clips" -> [(SoundClips, "")]
            "FLAC" -> [(FLAC, "")]
            "Other" -> [(Other, "")]
            _ -> []
