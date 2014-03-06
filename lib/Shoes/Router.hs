module Shoes.Router where

import Shoes.Conf
import Control.Monad.Reader
import Happstack.Server (dir, ok, seeOther, nullDir, trailingSlash, toResponse)
import Happstack.Server.Types (Response)
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.SimpleHTTP (ServerPart, ToMessage)

import Control.Monad (msum, MonadPlus, mzero, liftM)

home :: String
home = "list"

router :: AppConfReader (ServerPart String)
router = do
  homeFullPath <- mapReader ( ++ home) (asks urlBase)
  return $ msum [
      dir home $ ok "List"
      , dir "lololo" $ ok "Lololo"
      , seeOther homeFullPath homeFullPath
    ]