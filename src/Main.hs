module Main where

import Data.Foldable (traverse_)
import TPB

mkParams :: String -> Category -> Params
mkParams s c = Params opts []
  where
    opts = SearchOptions s c

main :: IO ()
main = do
    putStrLn "Enter an artist or song to search for:"
    s <- getLine
    putStrLn "Now choose a category:"
    traverse_ printCats (zip [1 ..] [minBound .. maxBound :: Category])
    c <- (toEnum @Category) . (read @Int) <$> getLine
    let params = mkParams s c
    p <- runTpb params search
    case p of
        Left r -> error $ show r
        Right (Results rs) -> traverse_ print rs

printCats :: (Int, Category) -> IO ()
printCats (x, y) = putStrLn $ show x ++ ") " ++ show y
