module Shoes.Conf where

import Control.Monad.Reader
import System.Environment(getEnvironment)
import System.Directory
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

createConf :: IO AppConf
createConf = do
  env <- getEnvironment
  let currDir = fromMaybe "/var/shoes/" (lookup workDirEnvKey env)
  setCurrentDirectory currDir
  return $ AppConf {
    urlBase = fromMaybe "/" (lookup urlBaseEnvKey env)
    , workDir = currDir
  }