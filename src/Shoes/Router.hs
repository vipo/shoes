module Shoes.Router where

import Shoes.Environment
import Shoes.Controller
import Control.Monad.Reader
import Happstack.Server (dir, seeOther, Response, toResponse)
import Happstack.Server.Types(Method(..))
import Happstack.Server.SimpleHTTP (ServerPart)
import Happstack.Server.Routing(methodM)

home :: String
home = "items"

router :: AppConf -> ServerPart Response
router conf = msum [
      dir home $ methodM GET >> listShoes conf
    , dir home $ methodM POST >> postShoeAsJson conf
    , seeOther homeFullPath (toResponse())
  ] where homeFullPath = (urlBase conf) ++ home
