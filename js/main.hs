{-# LANGUAGE QuasiQuotes #-}

module Main
    where

import Language.Javascript.JMacro


addLightbox :: JStat
addLightbox = [jmacro|
        $('img[alt^="img_"]').each(function() {
            $(this).parent().attr("data-lightbox", "gallery");
        });
|]


mainJs :: JStat
mainJs = [jmacro|jQuery(document).ready(function() { `(addLightbox)`; });|]

main :: IO ()
main = print $ renderJs mainJs

