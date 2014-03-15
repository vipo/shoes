module Shoes.Domain.Conf where

import Data.Acid(AcidState)
import Shoes.Domain.Model

data AppConf = AppConf {
  staticDir :: String
  , workDir :: String
  , imgsDir :: String
  , acidState :: AcidState ShoeStorage
}