{-# LANGUAGE Rank2Types #-}
module Utils
    ( takeRecentFirst
    ) where

import           Hakyll


takeRecentFirst :: Int -> (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
takeRecentFirst n = fmap (take n) . recentFirst



