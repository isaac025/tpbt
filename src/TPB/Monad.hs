module TPB.Monad where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson
import GHC.Generics (Generic)
import Network.HTTP.Simple
import TPB.Internals

mkReq :: (MonadIO m, MonadThrow m) => String -> m Request
mkReq s = parseRequest ("GET " <> api <> s)
data Song = Song
    { id :: String
    , name :: String
    , info_hash :: String
    , leechers :: String
    , seeders :: String
    , num_files :: String
    , size :: String
    , username :: String
    , added :: String
    , status :: String
    , category :: String
    , imdb :: String
    }
    deriving (Show, Generic)

instance ToJSON Song
instance FromJSON Song

class (Monad m) => TPBMonad m where
    search :: m ()

newtype TPB state = TPB {unTPB :: StateT [Song] IO state}
    deriving (Functor, Applicative, Monad, MonadIO)

instance TPBMonad TPB where
    search = TPB $ do
        liftIO (putStrLn "Please enter an artist/song:")
        s <- mkURL <$> liftIO getLine
        req <- liftIO (mkReq s)
        resp <- liftIO (httpJSON req :: IO (Response [Song]))
        liftIO $ print resp
