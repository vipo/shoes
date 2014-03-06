module Shoes.Conf where

import Control.Monad.Reader

type AppConfReader = Reader AppConf

data AppConf = AppConf {
  urlBase :: String
  , dataDir :: String
} deriving Show

defaultAppConf :: AppConf
defaultAppConf = AppConf {urlBase = "/", dataDir = "/var/shoes"}
