module Main where

import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Environment(run)

main :: IO ()
main = run ((simpleHTTP nullConf) . router)
