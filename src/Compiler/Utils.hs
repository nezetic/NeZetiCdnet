module Compiler.Utils
    ( groupByYear
    , loadAll'
    , minifyHtml
    , minifyJs
    , removeIndexHtml
    ) where

import           Data.Binary              (Binary)
import           Data.Function            (on)
import           Data.List                (groupBy, isInfixOf)
import           Data.Typeable            (Typeable)
import           System.FilePath.Posix    (splitFileName)

import           Hakyll


-- | Group items by year of their ISO @date@ metadata, preserving the
-- input order (feed it a newest-first list to get newest years first).
groupByYear :: MonadMetadata m => [Item a] -> m [(String, [Item a])]
groupByYear items = do
    years <- mapM itemYear items
    return [ (year, map snd grp)
           | grp@((year, _) : _) <- groupBy ((==) `on` fst) (zip years items) ]
  where
    itemYear item = maybe "" (take 4)
        <$> getMetadataField (itemIdentifier item) "date"

-- | Minify a page of HTML with the external @minify@ tool
-- (<https://github.com/tdewolff/minify>).  When reading from stdin the tool
-- cannot infer the filetype from a file name, so @--type=html@ is required.
minifyHtml :: Item String -> Compiler (Item String)
minifyHtml = withItemBody (unixFilter "minify" ["--type=html"])

-- | Minify a JavaScript file with the external @minify@ tool.  The scripts
-- are self-contained IIFEs that export nothing, so the default local-variable
-- mangling is safe; nothing relies on @eval@, @Function@ or @.name@.
minifyJs :: Compiler (Item String)
minifyJs = getResourceString >>= withItemBody (unixFilter "minify" ["--type=js"])

-- | Load all items matching a list of patterns.
loadAll' :: (Binary a, Typeable a) => [Pattern] -> Compiler [Item a]
loadAll' = concatMapM loadAll

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

-- | Replace URLs of the form @foo/bar/index.html@ with @foo/bar@.
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
    (dir, "index.html") | isLocal dir -> dir
                        | otherwise   -> url
    _                                 -> url
  where
    isLocal uri = not ("://" `isInfixOf` uri)
