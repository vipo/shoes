module Shoes.Environment where

import Control.Monad.Reader
import Control.Applicative((<*>))
import System.Environment(getEnvironment)
import System.Directory
import Control.Exception(bracket)
import Data.Maybe
import Data.Acid(AcidState)
import Data.Acid.Local(openLocalState, createCheckpointAndClose)
import Shoes.Storage

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
  let wd = fromMaybe "/var/tmp/shoes/" (lookup workDirEnvKey env)
  createDirectoryIfMissing False wd
  setCurrentDirectory wd
  return $ conf wd

initConf :: IO (String -> AcidState ShoeStorage -> AppConf)
initConf = do
  env <- getEnvironment
  let ub = fromMaybe "/" (lookup urlBaseEnvKey env)
  return $ AppConf ub

run :: (AppConf -> IO ()) -> IO ()
run action = bracket
  ((initConf >>= initWorkDir) <*> (openLocalState initialDataState))
  (createCheckpointAndClose . acidState)
  action