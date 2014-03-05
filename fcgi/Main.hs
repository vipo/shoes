module Main where

import System.Environment(getEnvironment)
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
    Nothing -> fail ("Environment variable not found: " ++ urlBaseEnvKey)
    Just (urlBase) -> runFastCGI $ serverPartToCGI $ router $ defaultAppConf {urlBase = urlBase}