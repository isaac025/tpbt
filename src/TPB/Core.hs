module TPB.Core (MonadTPB (..)) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson (FromJSON)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
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

processReq ::
    (MonadThrow m, MonadIO m, FromJSON a) =>
    ByteString ->
    String ->
    String ->
    String ->
    Maybe Category ->
    m a
processReq method qs q s mcat = do
    let url = mkURL qs q s mcat
    req <- mkRequest method url
    res <- getResults req
    case res of
        Right a -> pure a
        Left e -> throwM e

handler :: TPBError -> Tpb ()
handler e = liftIO $ print e

class MonadTPB m where
    pirateSearch :: m ()
    currentRes :: m ()
    resultContents :: Result -> m ()

instance MonadTPB Tpb where
    pirateSearch = do
        SearchFields{..} <- ask
        ( processReq "GET" "q.php" "?q=" search (Just searchCategory) >>= \e -> do
                Torrents{..} <- get
                put (Torrents{results = e, contents})
            )
            `catch` handler
    currentRes = do
        (Results res) <- gets results
        liftIO $ traverse_ printRes (zip [0 ..] res)
    resultContents Result{..} = undefined

printRes :: (Int, Result) -> IO ()
printRes (x, Result{name, id}) = putStrLn $ show x ++ ". " ++ name ++ ": " ++ id
