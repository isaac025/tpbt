module Main where

import Data.Foldable (traverse_)
import Data.Vector ((!))
import TPB
import Prelude hiding (getContents, id)

main :: IO ()
main = do
    putStrLn "Enter an artist or song to search for:"
    s <- getLine
    putStrLn "Now choose a category:"
    traverse_ printCats (zip [1 ..] [minBound .. maxBound :: Category])
    c <- (toEnum @Category) . (read @Int) <$> getLine
    let opts = SearchFields s c
    p <- runTpb opts pirateSearch
    case p of
        Left r -> error $ show r
        Right (Results rs) -> do
            let results = zip [1 .. 10] rs
            putStrLn "Here are the top ten results: "
            traverse_ printRes results
            putStrLn "Please select one to see the contents of it"
            s2 <- read @Int <$> getLine
            q <- runTpb opts (getContents (snd (results !! (s2 - 1))))
            case q of
                Left r -> error $ show r
                Right (Contents cs) -> traverse_ printConts (zip [1 ..] cs)

printCats :: (Int, Category) -> IO ()
printCats (x, y) = putStrLn $ show x ++ ") " ++ show y

printRes :: (Int, Result) -> IO ()
printRes (x, Result{name, id}) = putStrLn $ show x ++ ". " ++ name ++ ": " ++ id

printConts :: (Int, Content) -> IO ()
printConts (x, Content{cname}) = putStrLn $ show x ++ ". " ++ (cname ! 0)
