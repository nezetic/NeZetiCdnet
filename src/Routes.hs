module Routes
    ( niceRoute
    ) where

import           System.FilePath.Posix    (takeBaseName, takeDirectory, (</>))
import           Hakyll

-- replace a foo/bar.md by foo/bar/index.html
-- this way the url looks like: foo/bar in most browsers
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = let path    = toFilePath ident
                                 dir     = takeDirectory path
                                 setRoot = if dir /= "." then (dir </>) else id
        in setRoot $ takeBaseName path </> "index.html"


