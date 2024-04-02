{-# LANGUAGE ScopedTypeVariables #-}

module TPB.Core (MonadTPB (..)) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Network.HTTP.Simple (
    Request,
    getResponseBody,
    httpJSONEither,
    parseRequest,
    setRequestMethod,
 )
import TPB.Monad
import TPB.Types
import Prelude hiding (id)

api :: String
api = "https://apibay.org/q.php"

mkSearchURL :: String -> Category -> String
mkSearchURL s (toCatNum -> cat) = api <> s' <> "&cat=" <> cat
  where
    s' = "?q=" <> intercalate "%20" (words s)

mkRequest :: (MonadThrow m, MonadIO m) => ByteString -> String -> m Request
mkRequest method url = parseRequest url <&> setRequestMethod method

getResults :: (MonadIO m) => Request -> m (Either TPBError Results)
getResults req = do
    resp <- httpJSONEither req
    case getResponseBody resp of
        Left e -> pure (Left $ JSONFormat e)
        Right r -> pure (Right r)

class MonadTPB m where
    search :: m (Either TPBError Results)
    getContents :: String -> m (Either TPBError Results)

instance MonadTPB TpbM where
    search = do
        SearchOptions{..} <- grab @SearchOptions
        let url = mkSearchURL searchField searchCategory
        req <- liftIO $ mkRequest "GET" url
        liftIO $ getResults req
    getContents = undefined
