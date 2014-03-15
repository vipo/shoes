module Main where

import Shoes.Router(router)
import Shoes.Environment(run)
import Happstack.Server.FastCGI

main :: IO ()
main = run (runFastCGI . serverPartToCGI . router)