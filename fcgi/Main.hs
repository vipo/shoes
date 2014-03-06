module Main where

import Control.Monad.Reader
import Network.FastCGI(runFastCGI)
import Happstack.Server.FastCGI(serverPartToCGI)
import Shoes.Router(router)
import Shoes.Conf

main :: IO ()
main = createConf >>= runFastCGI . serverPartToCGI . (runReader router)
