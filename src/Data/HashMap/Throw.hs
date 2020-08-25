{- |
   Module     : Data.HashMap.Throw
   License    : MIT
   Stability  : experimental

lookupOrThrow function for HashMap.
-}
module Data.HashMap.Throw (
  lookupOrThrow
) where

import Control.Monad.Catch
import Data.Typeable
import Data.Hashable
import Data.HashMap as HM

newtype KeyNotFoundException a = KeyNotFoundException a
  deriving (Eq, Ord, Show)

instance (Typeable a, Show a) => Exception (KeyNotFoundException a)

lookupOrThrow :: (Eq a, Ord a, Show a, Typeable a, Hashable a, MonadThrow m) => a -> HashMap a b -> m b
lookupOrThrow k m = case HM.lookup k m of
                          Just x -> return x
                          Nothing -> throwM $ KeyNotFoundException k
