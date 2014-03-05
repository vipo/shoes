module Main where

import System.Environment(getEnvironment)
import System.Exit(exitWith, ExitCode(ExitFailure))
import Network.FastCGI(runFastCGI)
import Happstack.Server.FastCGI(serverPartToCGI)
import Shoes.Router(router)
import Shoes.Conf

urlBaseEnvKey :: String
urlBaseEnvKey = "SHOES_URL_BASE"

main :: IO ()
main = do
  env <- getEnvironment
  case (lookup urlBaseEnvKey env) of
    Nothing -> exitWith $ ExitFailure 42
    Just (ub) -> runFastCGI $ serverPartToCGI $ router $ defaultAppConf {urlBase = ub}
