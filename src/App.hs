module App where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Char (isDigit)
import Data.Text (Text, all)
import Say
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (getArgs)
import Text.Read
import Prelude hiding (all)

newtype MusicDirectory = MusicDirectory Text
    deriving (Show)

instance Read MusicDirectory where
    readPrec = do
        _ <- lexP >>= \s -> if s == "F" then pure () else error "does not start with F"
        digits <- lexP >>= \s -> if all isDigit s then pure s else error "Expected digits after F"
        pure $ MusicDirectory (pack $ "F" <> digits)

data AppEnvironment = AppEnvironment
    { ipod :: [MusicDirectory]
    , saveTo :: MusicDirectory
    , search :: Text
    , category :: Text
    }

newtype App a = App {unApp :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

makeNewSaveDir :: FilePath -> App ()
makeNewSaveDir f = do
    b <- liftIO $ doesDirectoryExist f
    unless b $
        say "Directory does not exist!"
    files <- liftIO $ listDirectory f
    let next = makeNextDirectory files
    pure ()

makeAppEnv :: FilePath -> FilePath -> App AppEnvironment
makeAppEnv _ _ = undefined

run :: App ()
run = do
    args <- liftIO getArgs
    case args of
        [f] -> do
            s <- makeNewSaveDir f
            env <- makeAppEnv f s
            liftIO $ say "created a directory"
        _ -> say "Usage: tpbt DIRECTORY"
