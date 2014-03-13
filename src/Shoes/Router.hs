module Shoes.Router where

import Shoes.Environment
import Shoes.Controller
import Control.Monad.Reader
import Happstack.Server (dir, seeOther)
import Happstack.Server.Types(Method(..))
import Happstack.Server.SimpleHTTP (ServerPart)
import Happstack.Server.Routing(methodM)

home :: String
home = "list"

router :: AppConf -> ServerPart String
router conf = msum [
      dir home $ methodM GET >> listShoes
    , dir home $ methodM POST >> postShoeAsJson conf
    , seeOther homeFullPath homeFullPath
  ] where homeFullPath = (urlBase conf) ++ home
