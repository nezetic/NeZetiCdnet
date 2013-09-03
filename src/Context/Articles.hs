module Context.Articles
    where

import           Data.Monoid              ((<>))
import           Hakyll


categoryCtx :: [Item String] -> Context String
categoryCtx posts =    listField "posts" defaultContext (return posts)
                    <> defaultContext


articlesCtx :: [Item String] -> Compiler [Item String] -> Context String
articlesCtx articles f =   constField "title" "Articles"
                      <> listField "articles" defaultContext (return articles)
                      <> listField "articlesbycategory" defaultContext f
                      <> defaultContext


articlesByCategoriesCtx :: [Item String] -> String -> String -> Context String
articlesByCategoriesCtx articles category url =   listField "posts" defaultContext (return articles)
                                               <> constField "category" category
                                               <> constField "url" url 
                                               <> constField "count" (show $ length articles)
                                               <> defaultContext

