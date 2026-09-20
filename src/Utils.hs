{-# LANGUAGE RankNTypes #-}
module Utils
    ( takeRecentFirst
    ) where

import           Hakyll


takeRecentFirst :: (MonadMetadata m, MonadFail m) => Int -> [Item a] -> m [Item a]
takeRecentFirst n = fmap (take n) . recentFirst
