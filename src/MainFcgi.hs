module Main where

import Control.Monad.Reader
import Shoes.Router(router)
import Shoes.Environment(run)
import Happstack.Server.FastCGI

main :: IO ()
main = run (runFastCGI . serverPartToCGI . router)