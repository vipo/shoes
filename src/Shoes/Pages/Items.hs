{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Items where

import Control.Monad (forM_, when)
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

import Shoes.Storage

page :: [ShoeData] -> H.Html
page shoes = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Shoes"
    H.link ! A.rel "stylesheet" ! A.href "http://yui.yahooapis.com/pure/0.4.2/pure-min.css"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
  H.body $ do
  	H.h1 "Items"
  	H.ul $ do
  	  forM_ shoes renderShoe

renderShoe :: ShoeData -> H.Html
renderShoe shoe = do
  H.li "fsfsdf"