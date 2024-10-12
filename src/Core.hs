{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Core where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Char (isAlphaNum)
import Data.IORef
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Dir = Dir
    { dir :: FilePath
    , saveAs :: IORef FilePath
    }

newtype App a = App {unApp :: ReaderT Dir IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Dir)

runApp :: Dir -> App a -> IO a
runApp d (App a) = runReaderT a d

data ReqEnv = ReqEnv
    { search :: Text
    , categorySel :: Audio
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
    deriving (Generic)

instance Show Result where
    show Result{..} = cleanedName <> " " <> prettySize <> "MB"
      where
        cleanedName = takeWhile (\x -> isAlphaNum x || x `elem` (" -" :: String)) name
        prettySize =
            let mb = show $ read @Double size * 1.048576
                (int, frac) = (takeWhile (/= '.') mb, take 3 $ dropWhile (/= '.') mb)
             in int <> frac

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
