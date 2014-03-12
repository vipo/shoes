module Main where

import Control.Monad.Reader
import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Environment(run)

main :: IO ()
main = run ((simpleHTTP nullConf) . (runReader router))
