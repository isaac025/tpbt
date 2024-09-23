{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module TPB.Types where

import Data.Aeson (FromJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, getResponseBody, httpJSON, parseRequest)
import Network.URI.Encode (encode)

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
    deriving (Generic)

instance Show Result where
    show Result{..} = name <> " " <> show (roundToTwo $ bytesToMegaBytes size) <> "Mb"
      where
        bytesToMegaBytes :: String -> Double
        bytesToMegaBytes (read -> bytes :: Int) = fromIntegral bytes / 1e6
        roundToTwo :: Double -> Double
        roundToTwo x = fromIntegral (round (x * 100)) / 100

instance FromJSON Result where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = camelTo2 '_'}

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
