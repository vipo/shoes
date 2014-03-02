module Main where

import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)

main :: IO ()
main = simpleHTTP nullConf $ router