{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, getResponseBody, httpJSON, parseRequest)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Prelude hiding (id)

{- data Video
    = Movies
    | MoviesDVDR
    | MusicVideos
    | MovieClips
    | TvShows
    | Handheld
    | HDMovies
    | HDTvShows
    | ThreeD
    | CamTS
    | UHDMovies
    | UHDTvShows
    | VOther
-}

api :: String
api = "https://apibay.org/q.php"

mkRequest :: String -> String -> IO Request
mkRequest c s = do
    let url = api <> "?" <> "q=" <> s <> "&" <> "cat=" <> c
    parseRequest ("GET " <> url)

fetchResults :: Request -> IO [Result]
fetchResults req = do
    response <- httpJSON req
    pure $ getResponseBody response
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

stdRead :: IO String
stdRead = putStr "search> " >> hFlush stdout >> getLine

main :: IO ()
main = do
    putStrLn "Welcome to The Pirate Bay TUI! (type 'quit' to quit at anytime)"
    repl
  where
    repl = do
        putStrLn "Please choose a category:"
        mapM_ putStrLn [y : ')' : ' ' : show x | (x, y) <- zip [Music .. Other] ['1' ..]]
        c <- stdRead
        when (c == "quit") exitSuccess
        let cat = toCat (read c :: Audio)
        putStrLn "Now search for anything:"
        s <- words <$> stdRead
        let search = intercalate "%20" s
        when (search == "quit") exitSuccess
        res <- fetchResults =<< mkRequest cat search
        mapM_ putStr [x : ')' : ' ' : show r | (r, x) <- zip res ['1' ..]]
        repl
