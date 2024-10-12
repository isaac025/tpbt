{-# LANGUAGE RecordWildCards #-}

module Lifted where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Core
import Data.IORef
import Data.Text (intercalate, pack)
import Network.HTTP.Simple (Request, getResponseBody, httpJSONEither, parseRequestThrow)
import Network.URI.Encode (encode)
import Say
import System.FilePath ((</>))
import System.Process
import Prelude hiding (id)

api :: String -> String
api f = "https://apibay.org/" <> f

makeRequest :: String -> String -> App Request
makeRequest s c = do
    let url = api "q.php" <> "?" <> "q=" <> s <> "&" <> "cat=" <> c
    liftIO $ parseRequestThrow ("GET " <> url)

fetch :: Request -> App [Result]
fetch req = do
    response <- getResponseBody <$> liftIO (httpJSONEither req)
    case response of
        Left _ -> do
            liftIO $ say "could not make a search parsing error"
            liftIO $ fail ""
        Right ls -> pure ls

printCategories :: App ()
printCategories = do
    let xs = [pack (show x) <> ") " <> pack (show cat) | (cat, x) <- zip [Music .. Other] ([1 ..] :: [Int])]
    liftIO $ say $ intercalate "\n" xs
    pure ()

printResults :: [Result] -> App ()
printResults res = do
    say "Please choose: "
    let xs = [pack (show x) <> ") " <> pack (show cat) | (cat, x) <- zip res ([1 ..] :: [Int])]
    liftIO $ say $ intercalate "\n" xs

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

downloadTorrent :: Result -> App ()
downloadTorrent r = do
    Dir{..} <- ask
    s' <- liftIO $ readIORef saveAs
    let path = dir </> s'
    let link = mkMagentLink r
    liftIO $ callProcess "transmission-cli" ["-w", path, link]
