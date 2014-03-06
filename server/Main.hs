module Main where

import Control.Monad.Reader
import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Conf

main :: IO ()
main = simpleHTTP nullConf $ (runReader router defaultAppConf)