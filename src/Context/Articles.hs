module Context.Articles
    where

import           Hakyll

import           Languages


-- Items in these lists need the language-aware date, hence @itemCtx@.
itemCtx :: Language -> Context String
itemCtx lang = languageCtx lang <> defaultContext


categoryCtx :: Language -> [Item String] -> Context String
categoryCtx lang posts = listField "posts" (itemCtx lang) (return posts)
                      <> languageCtx lang
                      <> defaultContext


articlesCtx :: Language -> [Item String] -> Compiler [Item String] -> Context String
articlesCtx lang articles f = constField "title" (trRecentArticles lang)
                      <> languageCtx lang
                      <> listField "articles" (itemCtx lang) (return articles)
                      <> listField "articlesbycategory" (itemCtx lang) f
                      <> defaultContext


articlesByCategoriesCtx :: Language -> [Item String] -> String -> String -> Context String
articlesByCategoriesCtx lang articles category url =
      listField "posts" (itemCtx lang) (return articles)
   <> constField "category" category
   <> constField "url" url
   <> constField "count" (show $ length articles)
   <> languageCtx lang
   <> defaultContext
