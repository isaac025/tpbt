{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), camelTo2, defaultOptions, fieldLabelModifier, genericParseJSON)
import Data.Functor ((<&>))
import Data.List (intercalate)
import GHC.Generics (Generic)
import Network.HTTP.Simple (Request, getResponseBody, httpJSON, parseRequest)
import Network.URI.Encode (encode)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Process (callProcess)
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

trackers :: String
trackers =
    concat
        [ "&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337"
        , "&tr=udp%3A%2F%2Fopen.stealth.si%3A80%2Fannounce"
        , "&tr=udp%3A%2F%2Ftracker.torrent.eu.org%3A451%2Fannounce"
        , "&tr=udp%3A%2F%2Ftracker.bittor.pw%3A1337%2Fannounce"
        , "&tr=udp%3A%2F%2Fpublic.popcorn-tracker.org%3A6969%2Fannounce"
        , "&tr=udp%3A%2F%2Ftracker.dler.org%3A6969%2Fannounce"
        , "&tr=udp%3A%2F%2Fexodus.desync.com%3A6969"
        , "&tr=udp%3A%2F%2Fopen.demonii.com%3A1337%2Fannounce"
        ]

mkMagentLink :: Result -> String
mkMagentLink Result{..} = base <> infoHash <> "&dn=" <> encode name <> trackers
  where
    base :: String
    base = "magnet:?xt=urn:btih:"

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
stdRead = putStr "tpbt> " >> hFlush stdout >> getLine

quit :: String -> IO ()
quit s = when (s == "quit") exitSuccess

chooseCategory :: IO String
chooseCategory = do
    putStrLn "Please choose a category:"
    mapM_ putStrLn [y : ')' : ' ' : show x | (x, y) <- zip [Music .. Other] ['1' ..]]
    c <- stdRead
    quit c
    pure $ toCat (read c :: Audio)

search :: IO String
search = do
    putStrLn "Now search for anything:"
    s <- words <$> stdRead
    let s' = intercalate "%20" s
    quit s'
    pure s'

chooseTorrent :: [(Int, Result)] -> IO (Maybe Result)
chooseTorrent [] = pure Nothing
chooseTorrent xs = do
    putStrLn "Please choose: "
    mapM_ putStrLn [show x <> ") " <> show r | (x, r) <- xs]
    sel <- read @Int <$> stdRead
    pure $ lookup sel xs

main :: IO ()
main = do
    putStrLn "Welcome to The Pirate Bay TUI! (type 'quit' to quit at anytime)"
    repl
  where
    repl = do
        cat <- chooseCategory
        s <- search
        res <- mkRequest cat s >>= fetchResults <&> zip [1 ..]
        tor <- chooseTorrent res
        case tor of
            Nothing -> putStrLn "No results found!" >> repl
            Just (mkMagentLink -> tor') -> callProcess "transmission-cli" [tor']
