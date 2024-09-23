module Main where

import Control.Monad (when)
import Data.Functor ((<&>))
import Data.List (intercalate)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Process (callProcess)
import TPB

stdRead :: IO String
stdRead = putStr "tpbt> " >> hFlush stdout >> getLine

quit :: String -> IO ()
quit s = when (s == "quit") exitSuccess

chooseCategory :: IO String
chooseCategory = do
    putStrLn "Please choose a category:"
    mapM_ putStrLn [y : ')' : ' ' : show x | (x, y) <- zip [Music .. Other] ['1' ..]]
    c <- stdRead
    quit c
    pure $ toCat (read c :: Audio)

search :: IO String
search = do
    putStrLn "Now search for anything:"
    s <- words <$> stdRead
    let s' = intercalate "%20" s
    quit s'
    pure s'

chooseTorrent :: [(Int, Result)] -> IO (Maybe Result)
chooseTorrent [] = pure Nothing
chooseTorrent xs = do
    putStrLn "Please choose: "
    mapM_ putStrLn [show x <> ") " <> show r | (x, r) <- xs]
    sel <- read @Int <$> stdRead
    pure $ lookup sel xs

main :: IO ()
main = do
    putStrLn "Welcome to The Pirate Bay TUI! (type 'quit' to quit at anytime)"
    repl
  where
    repl = do
        cat <- chooseCategory
        s <- search
        res <- mkRequest cat s >>= fetchResults <&> zip [1 ..]
        tor <- chooseTorrent res
        case tor of
            Nothing -> putStrLn "No results found!" >> repl
            Just (mkMagentLink -> tor') -> callProcess "transmission-cli" [tor']
