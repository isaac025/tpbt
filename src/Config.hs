{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Control.Monad.IO.Class

data AppEnvironment = AppEnvironment
    { ipod :: FilePath
    , saveTo :: FilePath
    }

newtype App a = App {unApp :: IO a}
    deriving (Functor, Applicative, Monad, MonadIO)
