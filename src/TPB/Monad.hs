{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TPB.Monad (
    mkRequest,
    fetch,
    downloadTorrent,
) where

import Data.Aeson (FromJSON)
import Data.Typeable (Typeable)
import Data.Vector (Vector, empty)
import Network.HTTP.Simple (Request, getResponseBody, httpJSONEither, parseRequestThrow)
import Network.URI.Encode (encode)
import System.Process (callProcess)
import TPB.Types
import Prelude hiding (id)

api :: String -> String
api f = "https://apibay.org/" <> f

mkRequest :: Either (String, String) Result -> IO Request
mkRequest (Left (s, c)) = do
    let url = api "q.php" <> "?" <> "q=" <> s <> "&" <> "cat=" <> c
    parseRequestThrow ("GET " <> url)
mkRequest (Right Result{..}) = do
    let url = api "f.php" <> "?" <> "id=" <> id
    parseRequestThrow ("GET " <> url)

fetch :: forall a. (FromJSON a, Typeable a) => Request -> IO (Vector a)
fetch req = do
    response <- getResponseBody <$> httpJSONEither req
    case response of
        Left _ -> pure empty
        Right ls -> pure ls

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

downloadTorrent :: Result -> IO ()
downloadTorrent = callProcess "transmission-cli" . (: []) . mkMagentLink
