module Shoes.Conf where

import Control.Monad.Reader
import System.Environment(getEnvironment)
import Data.Maybe

urlBaseEnvKey :: String
urlBaseEnvKey = "SHOES_URL_BASE"

workDirEnvKey :: String
workDirEnvKey = "SHOES_WORK_DIR"

type AppConfReader = Reader AppConf

data AppConf = AppConf {
  urlBase :: String
  , workDir :: String
} deriving Show

defaultAppConf :: AppConf
defaultAppConf = AppConf {urlBase = "/", workDir = "/var/shoes"}

createConf :: IO AppConf
createConf = do
  env <- getEnvironment
  return $ AppConf {
    urlBase = fromMaybe "/" (lookup urlBaseEnvKey env)
    , workDir = fromMaybe "/var/shoes" (lookup workDirEnvKey env)
  }