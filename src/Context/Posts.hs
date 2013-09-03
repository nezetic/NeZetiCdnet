module Context.Posts
    where

import           Data.Monoid              ((<>))
import           Hakyll

import Context.Teaser


postCtx :: Context String
postCtx  =    teaserCtx
           <> defaultContext


postsCtx :: [Item String] -> Context String
postsCtx posts =    constField "title" "All posts"
           <> listField "posts" defaultContext (return posts)
           <> defaultContext

