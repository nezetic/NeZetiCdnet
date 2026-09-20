{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List                (nub, sort)
import           GHC.IO.Encoding          (setLocaleEncoding, utf8)
import           Hakyll
import           Hakyll.Core.Dependencies  (DependencyKind (KindContent))

import           Compiler.Categories
import           Compiler.Utils
import           Config
import           Context.Articles
import           Context.Main
import           Context.Posts
import           Languages
import           Routes
import           Utils


main :: IO ()
main = do
    setLocaleEncoding utf8
    website


website :: IO ()
website = hakyllWith config $ do
    -- Language independent assets
    ["images/*", "images/*/*", "files/*", "robots.txt"] --> static
    match "js/*.js" $ route idRoute >> compile minifyJs

    -- Sitemap, covering every page of every language
    create ["sitemap.xml"] sitemap
    ["404.html"] --> raw
    match "css/*.css" $ route idRoute >> compile compressCssCompiler
    match "templates/*" $ compile templateCompiler

    -- One set of rules per language
    mapM_ languageRules languages

    where
        -- Tiny combinators to keep the rules section readable
        xs --> f = mapM_ (`match` f) xs

        -- French lives at the root, other languages below their prefix.  The
        -- source files of a language share that prefix, so a single set of
        -- rules works for every language.
        languageRules lang = do
            let p = langPrefix lang
            -- The language switcher links to the other language's version of a
            -- page.  Register that other content as a dependency so that
            -- adding or removing a translation invalidates this language's
            -- pages.
            deps <- mapM (makePatternDependency KindContent . fromGlob)
                        (translationPatterns lang)
            rulesExtraDependencies deps $ do
                create [fromFilePath (p ++ "index.html")] (index lang)
                create [fromFilePath (p ++ "posts.html")] (postsPage lang)
                match (fromGlob (p ++ "posts/*")) (post lang)
                match (fromGlob (p ++ "pages/*")) (pages lang)
                match (fromGlob (p ++ "projects/*")) (projects lang)
                match (fromGlob (p ++ "sidebar.markdown")) sidebar
                create [fromFilePath (p ++ "rss.xml")] (syndication lang renderRss)
                create [fromFilePath (p ++ "atom.xml")] (syndication lang renderAtom)

                -- Category pages
                categoriesTags <- createCategoriesTags
                    (p ++ "posts/*") (p ++ "categories/*.html")
                create [fromFilePath (p ++ "articles.html")] (articles lang categoriesTags)
                tagsRules categoriesTags (categories lang)

        -- Content patterns of the other language a page may link to.
        translationPatterns lang =
            let other = langPrefix (otherLanguage lang)
            in  [ other ++ "posts/*"
                , other ++ "pages/*"
                , other ++ "projects/*"
                ]

        -- Sitemap of all content pages, both languages included.  URLs are
        -- absolute, so no relativizeUrls here.
        sitemap = do
            route idRoute
            compile $ do
                content <- mapM (loadAll . fromGlob)
                    [p ++ pat | p <- ["", "en/"]
                              , pat <- [ "posts/*", "pages/*", "projects/*"
                                       , "categories/*.html"
                                       ]]
                    :: Compiler [[Item String]]
                created <- mapM (loadAll . fromGlob)
                    [ "index.html", "posts.html", "articles.html"
                    , "en/index.html", "en/posts.html", "en/articles.html"
                    ] :: Compiler [[Item String]]
                urls <- traverse itemUrl (concat (content ++ created))
                entries <- mapM makeItem (nub (sort urls))
                -- Inside $for(entries)$ only the list's context is in scope,
                -- so the root URL must be part of it.
                let entryCtx = bodyField "url" <> constField "root" rootUrl
                    sitemapCtx = listField "entries" entryCtx (return entries)
                              <> constField "root" rootUrl
                makeItem ("" :: String)
                    >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
          where
            itemUrl item = do
                mroute <- getRoute (itemIdentifier item)
                case mroute of
                    Nothing  -> fail "sitemap: item without route"
                    Just url -> return (siteUrl url)

        -- Copy a static file verbatim
        static = route idRoute >> compile copyFileCompiler

        -- The 404 page is served for any unknown URL, so relative links
        -- would resolve against that unknown path and 404 again: no
        -- relativizeUrls here.
        raw = do
            route idRoute
            compile $ do
                sidebody <- loadBody (fromFilePath
                    (langPrefix defaultLanguage ++ "sidebar.markdown"))
                projCtx <- projectsCtx defaultLanguage
                articleCompiler
                    >>= loadAndApplyTemplate "templates/default.html"
                            (sidebarCtx defaultLanguage sidebody
                                (projCtx <> defaultContext))
                    >>= minifyHtml

        sidebar = compile articleCompiler

        pages lang = do
            route $ gsubRoute "pages/" (const "") `composeRoutes` niceRoute
            compile $ articleCompiler
                >>= loadAndApplyTemplate "templates/page.html" pageCtx
                >>= finalize lang defaultContext
          where
            pageCtx = languageCtx lang <> defaultContext

        projects lang = do
            route niceRoute
            compile $ articleCompiler
                >>= loadAndApplyTemplate "templates/page.html" pageCtx
                >>= finalizeWith False lang defaultContext
          where
            pageCtx = languageCtx lang <> defaultContext

        postsPage lang = do
            route niceRoute
            compile $ do
                posts' <- recentFirst =<< loadAll (fromGlob (langPrefix lang ++ "posts/*"))
                years <- groupByYear posts'
                let postsCtx' = postsCtx lang years
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" postsCtx'
                    >>= finalize lang postsCtx'

        post lang = do
            route niceRoute
            compile $ do
                navCtx <- getUnderlying >>= navigationCtx lang
                articleCompiler
                    >>= saveSnapshot "content"
                    >>= loadAndApplyTemplate "templates/post.html"
                            (navCtx <> languageCtx lang <> defaultContext)
                    >>= finalize lang defaultContext

        articles lang categoriesTags = do
            route niceRoute
            compile $ do
                let tagPatternMap = patternsFromTags' categoriesTags
                arts <- takeRecentFirst 3 =<< loadPMap tagPatternMap
                let articlesCtx' =
                        articlesCtx lang arts
                            (articlesByCategories lang categoriesTags tagPatternMap
                                "templates/articlesbycategory.html")
                makeItem ""
                    >>= loadAndApplyTemplate "templates/articles.html" articlesCtx'
                    >>= finalize lang articlesCtx'
          where
            loadPMap = loadAll' . map snd

        categories lang _ pat = do
            route niceRoute
            compile $ do
                list <- recentFirst =<< loadAll pat
                let categoryCtx' = categoryCtx lang list
                makeItem ""
                    >>= loadAndApplyTemplate "templates/category.html" categoryCtx'
                    >>= finalize lang categoryCtx'

        createCategoriesTags postsPattern tagsPattern =
            buildTagsWith parseCategory (fromGlob postsPattern)
                (fromCapture (fromGlob tagsPattern))

        index lang = do
            route idRoute
            compile $ do
                posts' <- takeRecentFirst 4
                    =<< loadAllSnapshots (fromGlob (langPrefix lang ++ "posts/*")) "content"
                let indexCtx' = indexCtx lang posts'
                makeItem ""
                    >>= loadAndApplyTemplate "templates/index.html" indexCtx'
                    >>= finalize lang indexCtx'

        syndication lang renderer = do
            route idRoute
            compile $ do
                posts' <- takeRecentFirst 10
                    =<< loadAllSnapshots (fromGlob (langPrefix lang ++ "posts/*")) "content"
                renderer (feedConfiguration lang) (languageCtx lang <> defaultContext) posts'
                    >>= relativizeUrls

        -- Wrap a page in the site shell, then post-process the URLs.  When
        -- @listProjects@ is off (project pages) the sidebar projects section
        -- is skipped: the pages are members of that list and must not depend
        -- on it, or Hakyll would report a dependency cycle.
        finalize :: Language -> Context String -> Item String -> Compiler (Item String)
        finalize = finalizeWith True

        finalizeWith :: Bool -> Language -> Context String -> Item String
                     -> Compiler (Item String)
        finalizeWith listProjects lang ctx c = do
            sidebody <- loadBody (fromFilePath (langPrefix lang ++ "sidebar.markdown"))
            projCtx <- if listProjects then projectsCtx lang else return mempty
            loadAndApplyTemplate "templates/default.html"
                (sidebarCtx lang sidebody (projCtx <> ctx)) c
                >>= relativizeUrls >>= removeIndexHtml >>= minifyHtml
