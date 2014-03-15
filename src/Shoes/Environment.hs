module Shoes.Environment where

import Control.Applicative((<*>))
import System.Environment(getEnvironment)
import System.Directory
import System.FilePath.Posix(takeDirectory)
import Control.Exception(bracket)
import Data.Maybe
import Data.Acid(AcidState)
import Data.Acid.Local(openLocalState, createCheckpointAndClose)

import Shoes.Domain.Conf
import Shoes.Domain.Model
import Shoes.Acid(initialDataState)
import qualified Paths_shoes

workDirEnvKey :: String
workDirEnvKey = "SHOES_WORK_DIR"

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
  sd <- Paths_shoes.getDataFileName "static/flag"
  return $ AppConf (takeDirectory sd)

run :: (AppConf -> IO ()) -> IO ()
run action = bracket
  ((initConf >>= initDirs) <*> (openLocalState initialDataState))
  (createCheckpointAndClose . acidState)
  action
