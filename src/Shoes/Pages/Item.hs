{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Item(page) where

import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.String(fromString)

import Shoes.Domain.Model
import Shoes.Pages.Common(header)

page :: PersistedShoeData -> H.Html
page (PersistedShoeData _ shoe) = header $ do
  H.h1 (fromString(description shoe))
  H.div ! A.class_ "pure-g-r" $ do
    H.div ! A.class_ "pure-u-2-5" $ shoeImg shoe
    H.div ! A.class_ "pure-u-3-5" $ do
      H.p $ fromString ("Size: " ++ (size shoe))
      H.p $ fromString ("Color: " ++ (color shoe))

shoeImg :: ShoeData -> H.Html
shoeImg shoe = H.img !
  A.src (fromString("/img/" ++ (photoFileName shoe))) !
  A.alt (fromString(description shoe))