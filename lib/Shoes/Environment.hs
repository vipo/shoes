module Shoes.Environment where

import Control.Monad.Reader
import System.Environment(getEnvironment)
import System.Directory
import Control.Exception(bracket)
import Data.Maybe
import Data.Acid(AcidState)

import Shoes.Domain.Data

urlBaseEnvKey :: String
urlBaseEnvKey = "SHOES_URL_BASE"

workDirEnvKey :: String
workDirEnvKey = "SHOES_WORK_DIR"

type AppConfReader = Reader AppConf

data AppConf = AppConf {
  urlBase :: String
  , workDir :: String
  , acidState :: AcidState ShoeStorage
}

initWorkDir :: (String -> AcidState ShoeStorage -> AppConf) -> IO (AcidState ShoeStorage -> AppConf)
initWorkDir conf = do
  env <- getEnvironment
  let workDir = fromMaybe "/var/tmp/shoes/" (lookup workDirEnvKey env)
  createDirectoryIfMissing False workDir
  setCurrentDirectory workDir
  return $ conf workDir

initConf :: IO (String -> AcidState ShoeStorage -> AppConf)
initConf = do
  env <- getEnvironment
  let urlBase = fromMaybe "/" (lookup urlBaseEnvKey env)
  return $ AppConf urlBase
