{-# LANGUAGE ScopedTypeVariables #-}

module TPB.Core (MonadTPB (..)) where

import Control.Monad.IO.Class
import Data.Aeson (Array)
import Data.List (intercalate)
import Network.HTTP.Simple
import TPB.Monad
import TPB.Types

api :: String
api = "https://apibay.org/q.php"

class MonadTPB m where
    search :: m ()

instance MonadTPB TpbM where
    search = do
        SearchField p <- searchField <$> grab @SearchOptions
        let st = intercalate "%20" $ words p
        req <- liftIO $ parseRequest $ api <> "?q=" <> st
        resp :: Response Array <- liftIO $ httpJSON req
        let body = getResponseBody resp
        liftIO $ print body
