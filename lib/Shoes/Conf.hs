module Shoes.Conf where

data AppConf = AppConf {
  urlBase :: String
  , dataDir :: String
} deriving Show

defaultAppConf :: AppConf
defaultAppConf = AppConf {urlBase = "/", dataDir = "/var/shoes"}
