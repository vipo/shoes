module Main where

import Control.Monad.Reader
import Control.Applicative((<*>))
import Happstack.Server (nullConf, simpleHTTP)
import Shoes.Router(router)
import Shoes.Environment
import Shoes.Domain.Data(initialDataState)
import Control.Exception(bracket)
import Data.Acid.Local(openLocalState, createCheckpointAndClose)

main :: IO ()
main = bracket
  ((initConf >>= initWorkDir) <*> (openLocalState initialDataState))
  (createCheckpointAndClose . acidState)
  ((simpleHTTP nullConf) . (runReader router))