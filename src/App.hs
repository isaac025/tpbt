{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module App where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Core
import Data.Char (digitToInt)
import Data.IORef
import Data.Text (Text, pack, unpack)
import Data.Text.IO (getLine, putStr)
import Lifted
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

getNextDirectory :: [FilePath] -> FilePath
getNextDirectory [] = "F00"
getNextDirectory xs =
    let cdir = last xs
        (c, s) = (tail cdir, init cdir)
        n = digitToInt (last c) + 1
     in s <> show n

makeNewSaveDir :: FilePath -> IO (IORef FilePath)
makeNewSaveDir f = do
    b <- doesDirectoryExist f
    unless b $
        say "Directory does not exist!"
    files <- listDirectory f
    let next = getNextDirectory files
    withCurrentDirectory f $ do
        say $ "created directory: " <> pack next
        createDirectory next
    newIORef next

textRead :: App Text
textRead = liftIO (putStr "tpbt> ") >> liftIO (hFlush stdout) >> liftIO getLine

quit :: Text -> App ()
quit s = when (s == "quit" || s == "q") $ liftIO exitSuccess

updateNextDir :: App ()
updateNextDir = do
    Dir{..} <- ask
    paths <- liftIO $ listDirectory dir
    liftIO $ writeIORef saveAs (getNextDirectory paths)

repl :: App ()
repl = do
    liftIO $ say "Search for an artist/song: "
    s <- textRead
    quit s
    liftIO $ say "Select a category: "
    printCategories
    c <- textRead
    quit c
    req <- makeRequest (unpack s) (toCat (read @Audio $ unpack c))
    res <- fetch req
    printResults res
    num <- read @Int . unpack <$> textRead
    downloadTorrent (res !! num)
    -- updateNextDir
    repl

run :: IO ()
run = do
    args <- getArgs
    case args of
        [f] ->
            makeNewSaveDir f >>= \x -> runApp (Dir f x) repl
        _ -> say "Usage: tpbt DIRECTORY"
