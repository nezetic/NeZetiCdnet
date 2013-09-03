module Compiler.Js
    where

import           Data.ByteString.Lazy.UTF8 (toString, fromString)
import           Text.Jasmine (minify)
import           Hakyll


minifyJs :: Item String -> Compiler (Item String)
minifyJs item = return $ fmap minifyString item
    where minifyString = toString . minify . fromString


