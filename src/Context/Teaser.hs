module Context.Teaser
    ( teaserCtx
    ) where

import           Data.List                (isInfixOf)
import           Hakyll


teaserDelimiter :: String
teaserDelimiter = "<!--MORE-->"

noTeaserBegin :: String
noTeaserBegin = "<!--NOTEASERBEGIN-->"

noTeaserEnd :: String
noTeaserEnd = "<!--NOTEASEREND-->"


teaserCtx :: Context String
teaserCtx = field "teaser" teaser 
        where teaser item = let body = itemBody item
                            in if needTeaser body 
                                 then return (createTeaser body) 
                                 else fail "no teaser"
              needTeaser body = teaserDelimiter `isInfixOf` body


createTeaser :: String -> String
createTeaser = unlines . (noTeaser . extractTeaser') . lines
    where
        extractTeaser' = takeWhile (/= teaserDelimiter)

        noTeaser [] = []
        noTeaser (x : xs) 
            | x == noTeaserBegin = drop 1 $ dropWhile (/= noTeaserEnd) xs
            | otherwise = x : noTeaser xs


