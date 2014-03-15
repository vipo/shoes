{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Common(header) where

import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.String(fromString)

import Shoes.Environment

header :: AppConf -> H.Html -> H.Html
header conf body = H.docTypeHtml $ do
  H.head $ do
    H.title $ "Shoes"
    H.link !
      A.rel "stylesheet" !
      A.href (fromString ((urlBase conf) ++ "static/pure-min.css"))
  H.body body
