module Compiler.Utils
    ( loadAll'
    , removeIndexHtml
    ) where

import           Data.Binary              (Binary)
import           Data.Typeable            (Typeable)
import           Data.List                (isInfixOf)
import           System.FilePath.Posix    (splitFileName)

import           Hakyll


-- loadAll from a list of pattern
loadAll' :: (Binary a, Typeable a) => [Pattern] -> Compiler [Item a]
loadAll' = concatMapM loadAll

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

-- replace url of the form foo/bar/index.html by foo/bar
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item

removeIndexStr :: String -> String
removeIndexStr url = case splitFileName url of
    (dir, "index.html") | isLocal dir -> dir
                        | otherwise   -> url
    _                                 -> url
    where isLocal uri = not ("://" `isInfixOf` uri)

