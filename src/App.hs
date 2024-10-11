module App where

import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Data.Char (digitToInt)
import Data.Text (Text, pack)
import Data.Text.IO (getLine, putStr)
import Say
import System.Directory (
    createDirectory,
    doesDirectoryExist,
    listDirectory,
    withCurrentDirectory,
 )
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Prelude hiding (getLine, putStr)

data AppEnvironment = AppEnvironment
    { ipod :: FilePath
    , saveTo :: FilePath
    , search :: Text
    , category :: Text
    }

newtype App a = App {unApp :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

getNextDirectory :: [FilePath] -> FilePath
getNextDirectory [] = "F00"
getNextDirectory xs =
    let dir = last xs
        (c, s) = (tail dir, init dir)
        n = digitToInt (last c) + 1
     in s <> show n

makeNewSaveDir :: FilePath -> App FilePath
makeNewSaveDir f = do
    b <- liftIO $ doesDirectoryExist f
    unless b $
        say "Directory does not exist!"
    files <- liftIO $ listDirectory f
    let next = getNextDirectory files
    liftIO $ withCurrentDirectory f $ do
        say $ "created directory: " <> pack next
        createDirectory next
    pure next

textRead :: App Text
textRead = liftIO (putStr "tpbt> ") >> liftIO (hFlush stdout) >> liftIO getLine

quit :: Text -> App ()
quit s = when (s == "quit" || s == "q") $ liftIO exitSuccess

repl :: AppEnvironment -> App ()
repl a = do
    s <- textRead
    quit s
    liftIO $ say $ "you searched for: " <> s
    repl a

makeAppEnvrionment :: FilePath -> App AppEnvironment
makeAppEnvrionment f = do
    next <- makeNewSaveDir f
    pure $ AppEnvironment f next "" ""

run :: IO ()
run = do
    args <- getArgs
    case args of
        [f] -> do
            unApp (makeAppEnvrionment f) >>= unApp . repl
        _ -> say "Usage: tpbt DIRECTORY"
