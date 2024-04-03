module TPB.Core (MonadTPB (..)) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON)
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
api = "https://apibay.org/"

mkURL :: String -> String -> String -> Maybe Category -> String
mkURL qs q s Nothing = api <> qs <> s'
  where
    s' = q <> intercalate "%20" (words s)
mkURL qs q s (Just (toCatNum -> cat)) = api <> qs <> s' <> "&cat=" <> cat
  where
    s' = q <> intercalate "%20" (words s)

mkRequest :: (MonadThrow m, MonadIO m) => ByteString -> String -> m Request
mkRequest method url = parseRequest url <&> setRequestMethod method

getResults :: (MonadIO m, FromJSON a) => Request -> m (Either TPBError a)
getResults req = do
    resp <- httpJSONEither req
    case getResponseBody resp of
        Left e -> pure (Left $ JSONFormat e)
        Right r -> pure (Right r)

class MonadTPB m where
    pirateSearch :: m (Either TPBError Results)
    getContents :: Result -> m (Either TPBError Contents)

instance MonadTPB Tpb where
    pirateSearch = do
        SearchFields{..} <- ask
        let url = mkURL "q.php" "?q=" search (Just searchCategory)
        req <- liftIO $ mkRequest "GET" url
        liftIO $ getResults req
    getContents Result{..} = do
        let url = mkURL "f.php" "?id=" id Nothing
        liftIO $ print url
        req <- liftIO $ mkRequest "GET" url
        liftIO $ getResults req
