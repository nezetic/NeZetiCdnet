{-# LANGUAGE OverloadedStrings #-}
module Default 
    where

import Prelude           hiding  (div)
import Data.Monoid               ((<>))
import Clay 


main :: IO ()
main = do putCss theStylesheet
          putStrLn ""


theStylesheet :: Css
theStylesheet = 
    do html ?
         do backgroundColor white
            color black 
            sym margin 0
            sym padding 0
            defaultFont
       body ?
         do sym margin  0
            padding 0 0 60 0 
       theRoot
       theHeader
       theSidebar
       theSections
       theFooter


theRoot :: Css
theRoot = do
    theLinks
    theImages
    theTables
    theCodes
    theQuotes


theLinks :: Css
theLinks = do 
       a ? do (":link" <> ":visited") ? do 
                 color "#2b3e5f"
                 fontWeight bold
                 textDecoration none
              ":hover" ? textDecoration underline 
       a # ".right" ? float floatRight


theImages :: Css
theImages = do
       img ? border none (px 0) none
       img # "alt=img_left" ? (float floatLeft >> margin 5 12 5 0)
       img # "alt=img_right" ? (float floatRight >> margin 5 0 5 12)
       img # "alt=img_center" ? (display block >> sym2 margin 5 auto) 
       img # "alt=logo" ? (display block >> sym2 margin 10 auto) 


theSections :: Css
theSections =
    do div # "#main" ?
         do sym2 margin 0 auto
            width (px 860)
       div # "#content" ?
         do width (px 670)
            float floatRight
       theEntries


theEntries :: Css
theEntries = 
    do ul  # ".posts" ? ("list-style-type" -: "none")
       h2  # ".entrytitle" ? do
            sym margin 0
            sym2 padding 5 0
            entryTitleFont
       h3  # ".entrydate" ? do
            color "#888"
            dateFont
            textAlign (alignSide sideRight)
            marginBottom 3
       div # ".entrybody" ? do
            color "#424d53"
            entryBodyFont
            padding 5 0 10 5
            marginBottom 25
            borderBottom dashed (px 1) "#6d6d6f"
            p ? do sym2 margin 10 0
                   paddingLeft 0
            li ? ("list-style-type" -: "disc")
            ol ? do color "#777"
                    "list-style-position" -: "inside"
                    sym margin 0
                    sym padding 0
                    li ? do backgroundColor "#f9f9f9"
                            border solid (px 1) "#ebebeb"
       div # ".signature" ? textAlign (alignSide sideRight) 


theTables :: Css
theTables = 
    table ? do "border-collapse" -: "collapse"
               border solid (px 1) black
               sym margin 4
               td ? do border solid (px 1) black
                       sym padding 4
               th ? do border solid (px 2) black
                       sym padding 4

theCodes :: Css
theCodes = do
    code ? do codeFont
              display block
              sym padding 3
              backgroundColor "#eee"
              border solid (px 1) "#d3d3d6"
    pre ? do sym margin (px 0)
             code ? do
              marginTop (em (-1.0))
              marginBottom (em (-3.0))
              display block
              sym padding (px 8)

theQuotes :: Css
theQuotes =
    blockquote ? do backgroundColor "#f5f5ed"
                    border solid (px 1) "#e5e5dd"
                    color "#45453d"
                    fontQuotes
                    sym2 padding 3 10 
                    marginLeft 6


theHeader :: Css
theHeader = 
    div # "#header" ? 
         do margin 60 0 30 60
            height (px 75)
            headerFont
            a ? textDecoration none 
            backgroundImage (url "/images/code-small.png")
            backgroundRepeat noRepeat
            backgroundPosition (placed sideRight sideTop)


theFooter :: Css
theFooter = 
    div # "#footer" ?
         do paddingTop (px 30)
            clear both
            footerFont
            textAlign (alignSide sideCenter) 


theSidebar :: Css
theSidebar =
    div # "#sidebar" ? 
         do marginLeft (px 20)
            width (px 170)
            float floatRight
            a ? do clear both
                   display block
                   float floatRight
                   marginBottom (px 10)
                   textTransform capitalize
                   bigLinkFont
            h1 ? do color "#223"
                    clear both
                    float floatRight
                    textTransform uppercase
                    sidebarFont

-- Fonts
------------------------------------------
defaultFont :: Css
defaultFont = do fontFamily ["arial", "verdana"] [sansSerif] 
                 lineHeight (pct 140)


headerFont :: Css
headerFont = do fontFamily ["Finger Paint"] [cursive]
                fontWeight bold
                fontSize (px 70)

footerFont :: Css
footerFont = do fontFamily [] [sansSerif]
                fontWeight normal
                fontSize (pct 90)
                fontStyle italic

bigLinkFont :: Css
bigLinkFont = do fontFamily [] [sansSerif]
                 fontWeight normal
                 fontSize (px 18)

sidebarFont :: Css
sidebarFont = do fontFamily [] [sansSerif]
                 fontWeight normal
                 fontSize (px 24)

entryTitleFont :: Css
entryTitleFont = do fontFamily ["Century Gothic", "Lucida Grande"] [sansSerif]
                    fontWeight normal
                    fontSize (px 22)

entryBodyFont :: Css
entryBodyFont = do fontFamily [] [sansSerif]
                   fontWeight normal
                   fontSize (px 14)

dateFont :: Css
dateFont = do fontFamily [] [sansSerif]
              fontWeight normal
              fontSize (px 16)

codeFont :: Css
codeFont = do fontFamily ["lucida console", "Courier New"] [monospace]
              fontSize (px 12)
              fontWeight normal

fontQuotes :: Css
fontQuotes = do fontFamily [] [sansSerif]
                fontWeight normal
                fontStyle italic

