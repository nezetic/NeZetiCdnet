module Config
    ( config
    , feedConfiguration
    ) where

import           Hakyll

config :: Configuration
config = defaultConfiguration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "NeZetiC.net feed"
    , feedDescription = "NeZetiC.net RSS feed"
    , feedAuthorName  = "Cédric"
    , feedAuthorEmail = "nezetic @ gmail D0T com"
    , feedRoot        = "http://www.nezetic.net"
    }


