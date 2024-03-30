module TPB.Monad (
    Params (..),
    TpbM (..),
    Has (..),
    grab,
    runTpb,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import TPB.Types

data Params = Params
    { searchOpts :: SearchOptions
    , results :: [Result]
    }

newtype TpbM a = TpbM {runTpbM :: ReaderT Params IO a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Params)

class Has field env where
    obtain :: env -> field

instance Has SearchOptions Params where obtain = searchOpts
instance Has [Result] Params where obtain = results

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks obtain
{-# INLINE grab #-}

runTpb :: TpbM a -> Params -> IO a
runTpb tpb = runReaderT (runTpbM tpb)
