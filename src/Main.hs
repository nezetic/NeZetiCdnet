{-# LANGUAGE OverloadedStrings #-}
module Main where

import           GHC.IO.Encoding
import           Control.Applicative      ((<$>))
import           Hakyll


import Config
import Routes
import Context.Main
import Context.Posts
import Context.Articles
import Compiler.Html
import Compiler.Js
import Compiler.Categories
import Compiler.Utils
import Utils



main :: IO ()
main = do 
    setEncoding utf8
    website
    where setEncoding e = do
              setLocaleEncoding e
              setFileSystemEncoding e 
              setForeignEncoding e
 

website :: IO ()
website = hakyllWith config $ do
    ["index.html"]    -!> index
    ["posts.html"]    -!> posts
    ["posts/*"]       --> post
    ["pages/*"]       --> pages
    ["projects/*"]    --> projects
    ["images/*"]      --> static
    ["images/*/*"]    --> static
    ["files/*"]       --> static
    ["js/*.js"]       --> static
    ["404.html"]      --> raw
    ["rss.xml"]       -!> syndication renderRss
    ["atom.xml"]      -!> syndication renderAtom

    -- Categories
    categoriesTags    <-  createCategoriesTags "posts/*" "categories/*.html"
    ["articles.html"] -!> articles categoriesTags
    categoriesTags    -@> categories

    -- Compress CSS
    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    -- Add Clay-based CSS 
    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ fmap compressCss <$> fromGhc

    -- Add JMacro based javascript
    match "js/*.hs" $ do
        route   $ setExtension "js"
        compile $ minifyJs =<< fromGhc

    -- Sidebar
    match "sidebar.markdown" $ compile pandocCompiler

    -- Templates
    match "templates/*" $ compile templateCompiler

    where
         -- Useful combinators here
        xs --> f = mapM_ (`match` f) xs
        xs -!> f = create xs f 
        tags -@> f = tagsRules tags f

        -- Static Routes Generator
        static = route idRoute >> compile copyFileCompiler
        
        fromGhc = getResourceString >>= withItemBody (unixFilter "runghc" [])

        -- Raw Routes Generator (no templates)
        raw = do 
            route idRoute
            compile $ pandocCompiler >>= finalize

        pages = do
            route $ gsubRoute "pages/" (const "") `composeRoutes` niceRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= finalize

        projects = do
            route niceRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" defaultContext
                >>= finalize

        posts = do 
            route niceRoute 
            compile $ do 
                posts' <- recentFirst =<< loadAll "posts/*"
                let postsCtx' = postsCtx posts' 
                makeItem ""
                    >>= loadAndApplyTemplate "templates/posts.html" postsCtx'
                    >>= finalize_ postsCtx'

        post = do 
            route niceRoute
            compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" defaultContext
                >>= finalize

        articles categoriesTags = do 
            route niceRoute
            compile $ do
                let tagPatternMap = patternsFromTags' categoriesTags
                arts <- takeRecentFirst 3 =<< loadPMap tagPatternMap
                let articlesCtx' = articlesCtx arts (articlesByCategories categoriesTags tagPatternMap "templates/articlesbycategory.html")
                makeItem ""
                    >>= loadAndApplyTemplate "templates/articles.html" articlesCtx'
                    >>= finalize_ articlesCtx'
                where loadPMap = loadAll'. map snd

        categories _ pattern = do
            route niceRoute
            compile $ do
                list <- recentFirst =<< loadAll pattern
                makeItem ""
                    >>= loadAndApplyTemplate "templates/category.html" (categoryCtx list)
                    >>= finalize 

        createCategoriesTags postsPattern tagsPattern = buildTagsWith parseCategory postsPattern (fromCapture tagsPattern)
 
        index = do 
            route idRoute
            compile $ do
                posts' <- takeRecentFirst 4 =<< loadAllSnapshots "posts/*" "content"
                let indexCtx' = indexCtx posts'
                makeItem ""
                    >>= loadAndApplyTemplate "templates/index.html" indexCtx'
                    >>= finalize_ indexCtx'

        syndication renderer = do
            route idRoute
            compile $ do
                posts' <- takeRecentFirst 10 =<< loadAllSnapshots "posts/*" "content"
                renderer feedConfiguration defaultContext posts'
                >>= relativizeUrls 

        finalize' ctx c = do sidebody <- loadBody "sidebar.markdown"
                             loadAndApplyTemplate "templates/default.html" (sidebarCtx sidebody ctx) c 
        
        finalize c = finalize' defaultContext c >>= relativizeUrls >>= removeIndexHtml >>= tidyHtml
        finalize_ ctx c = finalize' ctx c >>= relativizeUrls >>= removeIndexHtml >>= tidyHtml


