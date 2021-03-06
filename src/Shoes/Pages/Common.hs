{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Common(header) where

import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A

header :: H.Html -> H.Html
header body = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Shoes"
    H.link !
      A.rel "stylesheet" !
      A.href "/static/pure-min.css"
  H.body body
