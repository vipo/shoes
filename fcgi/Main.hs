module Main where

import Control.Monad.Reader
import Control.Applicative((<*>))
import Shoes.Router(router)
import Shoes.Environment
import Shoes.Domain.Data(initialDataState)
import Control.Exception(bracket)
import Data.Acid.Local(openLocalState, createCheckpointAndClose)
import Happstack.Server.FastCGI

main :: IO ()
main = bracket
  ((initConf >>= initWorkDir) <*> (openLocalState initialDataState))
  (createCheckpointAndClose . acidState)
  (runFastCGI . serverPartToCGI . (runReader router))