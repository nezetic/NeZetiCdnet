module Config
    ( config
    , rootUrl
    , feedConfiguration
    , articleCompiler
    ) where

import           Hakyll
import           Languages
import           Text.Pandoc            (ReaderOptions (..))
import           Text.Pandoc.Extensions (Extension (Ext_implicit_figures),
                                         disableExtension)

-- | Absolute URL the site is served from (no trailing slash).
rootUrl :: String
rootUrl = "https://nezetic.net"

config :: Configuration
config = defaultConfiguration
    { deployCommand = "rsync -a --filter='P _site/' --filter='P _cache/' --filter='P .git/' --filter='P .gitignore' --filter='P README.md' --filter='P .stack-work' --delete-excluded _site/ nezetic.github.io"
    }

feedConfiguration :: Language -> FeedConfiguration
feedConfiguration lang = FeedConfiguration
    { feedTitle       = trFeedTitle lang
    , feedDescription = trFeedDescription lang
    , feedAuthorName  = "Cédric"
    , feedAuthorEmail = ""
    , feedRoot        = rootUrl
    }

-- | Pandoc's @implicit_figures@ extension turns a lone image into a
-- @<figure>@ and renders its alt text as a visible caption.  The images on
-- this site use the alt text as a CSS hook (@img_right@, @img_left@, …),
-- so the caption must not be displayed.  Disable the extension once here and
-- keep using 'articleCompiler' instead of 'pandocCompiler' everywhere.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions
    { readerExtensions = disableExtension Ext_implicit_figures
                            (readerExtensions defaultHakyllReaderOptions) }

articleCompiler :: Compiler (Item String)
articleCompiler = pandocCompilerWith readerOptions defaultHakyllWriterOptions


