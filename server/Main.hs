module Main where

import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Conf

main :: IO ()
main = simpleHTTP nullConf $ router defaultAppConf