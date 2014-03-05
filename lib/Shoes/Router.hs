module Shoes.Router where

import Shoes.Conf
import Happstack.Server (dir, ok, seeOther, nullDir, trailingSlash, toResponse)
import Happstack.Server.Types (Response)
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.SimpleHTTP (ServerPart, ToMessage)

import Control.Monad (msum, MonadPlus, mzero, liftM)

home :: String
home = "list"

router :: AppConf -> ServerPart String
router conf = msum [
    dir home $ ok "List"
    , dir "lololo" $ ok "Lololo"
    , seeOther defaultRedirectTo defaultRedirectTo
  ] where defaultRedirectTo = (urlBase conf) ++ home
