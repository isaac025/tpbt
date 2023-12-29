module Main where

import Control.Monad (void)
import Control.Monad.State
import TPB.Monad

main :: IO ()
main = void $ flip runStateT [] $ unTPB search
