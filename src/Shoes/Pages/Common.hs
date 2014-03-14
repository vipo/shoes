{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Common where

import Control.Monad (forM_, when)
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

wrapper :: H.Html -> H.Html
wrapper body = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Shoes"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
 H.body body
