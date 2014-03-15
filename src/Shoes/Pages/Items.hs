{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Items(page) where

import Control.Monad (forM_)
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.String(fromString)

import Shoes.Storage
import Shoes.Pages.Common(header)
import Shoes.Environment

page :: AppConf -> [ShoeData] -> H.Html
page conf shoes = header conf $ do
  H.h1 "Items"
  H.ul $ do
    forM_ shoes renderShoe

renderShoe :: ShoeData -> H.Html
renderShoe shoe = do
  H.li $ H.a ! A.href (fromString ("item/" ++ (show (shoeId shoe)))) $
    (fromString (description shoe))