{-# LANGUAGE OverloadedStrings #-}

module Compiler.Categories
    ( parseCategory
    , articlesByCategories
    , patternsFromTags
    , patternsFromTags'
    ) where

import           Control.Monad            (liftM, forM)
import           Control.Arrow            (second)
import           Data.Maybe               (fromMaybe)
import           Hakyll

import Context.Articles


parseCategory :: MonadMetadata m => Identifier -> m [String]
parseCategory identifier = return . toCategoryList . lookupString "category" =<< getMetadata identifier
    where toCategoryList = maybe [] ((:[]) . trim)


articlesByCategories :: Tags -> [(String, Pattern)] -> Identifier -> Compiler [Item String]
articlesByCategories tags pxs tmplt = forM pxs $ \(tag, pattern) -> do
                                  catArticles <- recentFirst =<< loadAll pattern 
                                  catUrl <- urlFromTag tag
                                  let catCtx = articlesByCategoriesCtx catArticles tag catUrl 
                                  makeItem "" >>= loadAndApplyTemplate tmplt catCtx
            where urlFromTag = liftM ensureRoot . getRoute . tagsMakeId tags
                  ensureRoot = ('/' :) . fromMaybe ""
                               

patternsFromTags' :: Tags -> [(String, Pattern)]
patternsFromTags' tags = map (second fromList) (tagsMap tags)

patternsFromTags :: Tags -> [Pattern]
patternsFromTags = snd . unzip . patternsFromTags'

