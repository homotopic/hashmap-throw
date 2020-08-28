{- |
   Module     : Data.HashMap.Throw
   License    : MIT
   Stability  : experimental

lookupOrThrow function for HashMap.
-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Data.HashMap.Throw (
  lookupOrThrow
, KeyNotFoundException(..)
) where

import Control.Monad.Catch
import Data.Typeable
import Data.Hashable
import Data.HashMap as HM

-- | Exception thrown when a key is not found in a hashmap.
newtype KeyNotFoundException a = KeyNotFoundException a
  deriving (Eq, Ord, Show)

instance (Typeable a, Show a) => Exception (KeyNotFoundException a)

-- | `HM.lookup` lifted to `MonadThrow` that throws `KeyNotFoundException`.
lookupOrThrow :: (Eq a, Ord a, Show a, Typeable a, Hashable a, MonadThrow m) => a -> HashMap a b -> m b
lookupOrThrow k m = case HM.lookup k m of
                          Just x -> return x
                          Nothing -> throwM $ KeyNotFoundException k
