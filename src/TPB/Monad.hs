{-# LANGUAGE DerivingVia #-}

module TPB.Monad (
    SearchFields (..),
    Torrents (..),
    Tpb (..),
    runTpb,
) where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid (Ap (..))
import Data.Vector (empty)
import TPB.Types

-- | Search fields to pass to the pirate bay api
data SearchFields = SearchFields
    { search :: String
    , searchCategory :: Category
    }

-- | Torrents, downloaded data from the pirate bay
data Torrents = Torrents
    { results :: Results
    , contents :: Contents
    }

newtype Tpb a = Tpb (ReaderT SearchFields (StateT Torrents IO) a)
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadReader SearchFields, MonadState Torrents)
    deriving (Semigroup, Monoid) via Ap Tpb a

runTpb :: SearchFields -> Tpb a -> IO a
runTpb p (Tpb a) = evalStateT (runReaderT a p) emptyTorrents
  where
    emptyTorrents = Torrents (Results empty) (Contents [])
