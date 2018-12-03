module Config
    ( config
    , feedConfiguration
    ) where

import           Hakyll

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -a --filter='P _site/' --filter='P _cache/' --filter='P .git/' --filter='P .gitignore' --filter='P README.md' --filter='P .stack-work' --delete-excluded _site/ nezetic.github.io"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "NeZetiC.net feed"
    , feedDescription = "NeZetiC.net RSS feed"
    , feedAuthorName  = "Cédric"
    , feedAuthorEmail = "nezetic @ gmail D0T com"
    , feedRoot        = "http://www.nezetic.net"
    }


