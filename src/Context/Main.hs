module Context.Main
    where

import           Data.Monoid              ((<>))
import           Hakyll

import Context.Posts


indexCtx :: [Item String] -> Context String
indexCtx posts =  constField "title" "Home"
          <> listField  "posts" postCtx (return posts)
          <> defaultContext


sidebarCtx :: String -> Context a -> Context a
sidebarCtx body linkedCtx = constField "sidebar" body <> linkedCtx

