module Context.Posts
    where

import           Control.Monad            (forM)
import           Data.List                (elemIndex, sortOn)
import           Data.Maybe               (listToMaybe)
import           Data.Ord                 (Down (..))
import           Hakyll

import           Context.Teaser
import           Languages


postCtx :: Language -> Context String
postCtx lang = languageCtx lang
      <> teaserCtx
      <> defaultContext


postsCtx :: Language -> [(String, [Item String])] -> Context String
postsCtx lang years = constField "title" (trAllWritings lang)
              <> languageCtx lang
              <> listField "years" (yearCtx lang) (traverse makeItem years)
              <> defaultContext


-- | Context of one year group on the posts page: the year and its posts.
yearCtx :: Language -> Context (String, [Item String])
yearCtx lang = field "year" (return . fst . itemBody)
            <> listFieldWith "posts" (postCtx lang) (return . snd . itemBody)


-- | Context linking to the previous (older) and next (newer) post around
-- the given one.  Posts are ordered by their ISO @date@ metadata, so plain
-- string comparison is chronological.  Only metadata and routes are read,
-- not the posts themselves: loading them here would create a circular
-- dependency while a post is being compiled.
navigationCtx :: Language -> Identifier -> Compiler (Context String)
navigationCtx lang current = do
    idents <- getMatches (fromGlob (langPrefix lang ++ "posts/*"))
    dated <- forM idents $ \ident -> do
        date <- getMetadataField' ident "date"
        return (date, ident)
    let sorted = map snd (sortOn (Down . fst) dated)   -- newest first
        (older, newer) = case elemIndex current sorted of
            Nothing -> (Nothing, Nothing)
            Just i  -> (at (i + 1) sorted, at (i - 1) sorted)
    prevCtx <- neighbourCtx "prev" older
    nextCtx <- neighbourCtx "next" newer
    return (prevCtx <> nextCtx)
  where
    -- Element at a (possibly out of range or negative) index.
    at i xs | i < 0     = Nothing
            | otherwise = listToMaybe (drop i xs)


-- Context fields @<key>_url@ and @<key>_title@ for a neighbouring post.
neighbourCtx :: String -> Maybe Identifier -> Compiler (Context String)
neighbourCtx _    Nothing     = return mempty
neighbourCtx key (Just ident) = do
    title  <- getMetadataField' ident "title"
    mroute <- getRoute ident
    return $ case mroute of
        Nothing  -> mempty
        Just url ->      constField (key ++ "_url") (siteUrl url)
                    <> constField (key ++ "_title") title
