{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TPB.Monad where

import Network.HTTP.Simple (Request, getResponseBody, httpJSON, parseRequest)
import Network.URI.Encode (encode)
import TPB.Types

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
