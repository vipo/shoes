module Main where

import Network.FastCGI(runFastCGI)
import Happstack.Server.FastCGI(serverPartToCGI)
import Shoes.Router(router)

main :: IO ()
main = runFastCGI $ serverPartToCGI $ router 