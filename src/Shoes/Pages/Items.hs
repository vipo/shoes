{-# LANGUAGE OverloadedStrings #-}
module Shoes.Pages.Items(page) where

import Control.Monad (forM_)
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.String(fromString)

import Shoes.Domain.Model
import Shoes.Pages.Common(header)

page :: [PersistedShoeData] -> H.Html
page shoes = header $ do
  H.h1 "Items"
  H.ul $ do
    forM_ shoes renderShoe

renderShoe :: PersistedShoeData -> H.Html
renderShoe (PersistedShoeData i s) = do
  H.li $ H.a ! A.href (fromString ("/item/" ++ show (i))) $
    (fromString (description s))