module Compiler.Html
    ( tidyHtml
    ) where

import           Data.List                (intercalate)
import           Text.XML.HXT.Core
import           Hakyll


tidyHtml :: Item String -> Compiler (Item String)
tidyHtml item = return $ fmap tidyHtml' item


tidyHtml' :: String -> String
tidyHtml' html = let doc       = selem "/" [hread]
                     toString  = writeDocumentToString [withOutputXHTML, withIndent yes, withXmlPi no]
                     formatted = runLA (doc >>> removeAllComment >>> addXHtmlDoctypeStrict >>> toString) html
                 in intercalate "\n" formatted


