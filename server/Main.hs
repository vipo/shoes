module Main where

import Control.Monad.Reader
import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Conf

main :: IO ()
main = createConf >>= (simpleHTTP nullConf) . (runReader router)