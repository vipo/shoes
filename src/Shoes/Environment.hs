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
  , imgsDir :: String
  , acidState :: AcidState ShoeStorage
}

initDirs :: (String -> String -> AcidState ShoeStorage -> AppConf) -> IO (AcidState ShoeStorage -> AppConf)
initDirs conf = do
  env <- getEnvironment
  let workd = fromMaybe "/var/tmp/shoes/" (lookup workDirEnvKey env)
  createDirectoryIfMissing False workd
  setCurrentDirectory workd
  let imgsd = workd ++ "imgs/"
  createDirectoryIfMissing False imgsd
  return $ conf workd imgsd

initConf :: IO (String -> String -> AcidState ShoeStorage -> AppConf)
initConf = do
  env <- getEnvironment
  let ub = fromMaybe "/" (lookup urlBaseEnvKey env)
  return $ AppConf ub

run :: (AppConf -> IO ()) -> IO ()
run action = bracket
  ((initConf >>= initDirs) <*> (openLocalState initialDataState))
  (createCheckpointAndClose . acidState)
  action