{-# LANGUAGE DerivingVia #-}

module TPB.Monad (
    SearchFields (..),
    Tpb (..),
    runTpb,
) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid (Ap (..))
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
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader SearchFields, MonadState Torrents)
    deriving (Semigroup, Monoid) via Ap Tpb a

runTpb :: SearchFields -> Tpb a -> IO a
runTpb p (Tpb a) = evalStateT (runReaderT a p) emptyTorrents
  where
    emptyTorrents = Torrents (Results []) (Contents [])
