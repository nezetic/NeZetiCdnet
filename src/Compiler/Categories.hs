module Compiler.Categories
    ( parseCategory
    , articlesByCategories
    , patternsFromTags
    , patternsFromTags'
    ) where

import           Control.Arrow            (second)
import           Control.Monad            (forM)
import           Data.Maybe               (fromMaybe)
import           Hakyll

import           Context.Articles
import           Languages


parseCategory :: MonadMetadata m => Identifier -> m [String]
parseCategory identifier =
    toCategoryList . lookupString "category" <$> getMetadata identifier
  where
    toCategoryList = maybe [] ((:[]) . trim)


articlesByCategories :: Language -> Tags -> [(String, Pattern)] -> Identifier -> Compiler [Item String]
articlesByCategories lang tags pxs tmplt = forM pxs $ \(tag, pat) -> do
    catArticles <- recentFirst =<< loadAll pat
    catUrl <- urlFromTag tag
    let catCtx = articlesByCategoriesCtx lang catArticles tag catUrl
    makeItem "" >>= loadAndApplyTemplate tmplt catCtx
  where
    urlFromTag = fmap ensureRoot . getRoute . tagsMakeId tags
    ensureRoot = ('/' :) . fromMaybe ""


patternsFromTags' :: Tags -> [(String, Pattern)]
patternsFromTags' tags = map (second fromList) (tagsMap tags)


patternsFromTags :: Tags -> [Pattern]
patternsFromTags = map snd . patternsFromTags'
