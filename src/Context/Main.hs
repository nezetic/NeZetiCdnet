module Context.Main
    where

import           Control.Monad              (filterM)

import           Hakyll

import           Config
import           Context.Posts
import           Languages


indexCtx :: Language -> [Item String] -> Context String
-- 'title' is deliberately absent: the home page's @<title>@ is just the
-- site name, see the conditional in templates/default.html.
indexCtx lang posts = languageCtx lang
              <> listField  "posts" (postCtx lang) (return posts)
              <> defaultContextNoTitle

-- 'defaultContext' without its 'titleField', which would fall back to the
-- item's file name for pages without a title in their metadata.
defaultContextNoTitle :: Context String
defaultContextNoTitle =
    metadataField
        <> urlField "url"
        <> pathField "path"
        <> bodyField "body"
        <> missingField


sidebarCtx :: Language -> String -> Context a -> Context a
sidebarCtx lang body linkedCtx =
    languageCtx lang <> seoCtx lang <> constField "sidebar" body <> linkedCtx


-- | Absolute URLs for search engines: a self-referential canonical URL and
-- the @hreflang@ alternates telling crawlers that the two languages are
-- translations rather than duplicates.  They are emitted absolute so that
-- 'relativizeUrls' leaves them alone.
--
-- Defined here rather than in "Languages" only because it needs 'rootUrl'.
--
-- The alternate tags are only produced for pages that really have a
-- translation: 'translationUrlMaybe' returning 'Nothing' yields the empty
-- string, so the tag disappears.  (A "$if(field)$" would not work here: a
-- field that exists but is empty is truthy in Hakyll templates.)  Falling
-- back to the other language's home page, as the switcher does, would create
-- non-reciprocal @hreflang@ clusters.
seoCtx :: Language -> Context a
seoCtx lang =
       field "canonical_tags" (\item -> do
            murl <- absoluteMaybe canonicalUrlMaybe item
            return $ maybe "" (\url ->
                linkTag "canonical" Nothing url ++ "\n"
                ++ linkTag "alternate" (Just (langCode lang)) url) murl)
    <> field "alt_tags" (\item -> do
            murl <- absoluteMaybe (translationUrlMaybe lang other) item
            return $ maybe "" (linkTag "alternate" (Just (langCode other))) murl)
    <> field "default_tags" (\item -> do
            murl <- absoluteMaybe defaultLookup item
            return $ maybe "" (linkTag "alternate" (Just "x-default")) murl)
  where
    other = otherLanguage lang
    -- x-default is the default language's version of the page.  On a page
    -- already in the default language that is the page itself, so take the
    -- canonical URL directly instead of round-tripping through the prefix
    -- dance.  Either way every page of a cluster advertises the same
    -- x-default URL (see the note above).
    defaultLookup
        | langCode lang == langCode defaultLanguage = canonicalUrlMaybe
        | otherwise = translationUrlMaybe lang defaultLanguage
    absoluteMaybe f item = fmap (fmap (rootUrl ++)) (f item)
    linkTag rel mhreflang url =
        "<link rel=\"" ++ rel ++ "\""
            ++ maybe "" (\l -> " hreflang=\"" ++ l ++ "\"") mhreflang
            ++ " href=\"" ++ escapeHtml url ++ "\">"

-- | The projects listed in the sidebar: the non-archived ones of a language
-- (marked @archived: true@ in their metadata are still published, but hidden
-- from the navigation).  'mempty' when there is none, so that the whole
-- "Projects" section disappears from the sidebar.
--
-- This is deliberately kept out of 'sidebarCtx': the project pages are
-- members of the list and must not depend on it, or Hakyll would report a
-- dependency cycle.
projectsCtx :: Language -> Compiler (Context String)
projectsCtx lang = do
    projects' <- activeProjects lang
    if null projects'
        then return mempty
        else return $ constField "projects_heading" (trProjects lang)
                   <> listField "projects" projectCtx (return projects')
  where
    projectCtx = metadataField <> urlField "url" <> missingField

activeProjects :: Language -> Compiler [Item String]
activeProjects lang = do
    all' <- loadAll (fromGlob (langPrefix lang ++ "projects/*"))
    filterM (fmap (not . isArchived) . getMetadata . itemIdentifier) all'
  where
    isArchived md = lookupString "archived" md == Just "true"
