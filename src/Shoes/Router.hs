module Shoes.Router where

import Shoes.Domain.Conf
import Shoes.Controller
import Control.Monad.Reader
import Happstack.Server (dir, seeOther, Response, toResponse, path, serveDirectory, Browsing(DisableBrowsing))
import Happstack.Server.Types(Method(..))
import Happstack.Server.SimpleHTTP (ServerPart)
import Happstack.Server.Routing(methodM)

home :: String
home = "items"

router :: AppConf -> ServerPart Response
router conf = msum [
      dir home $ methodM GET >> listShoes conf
    , dir home $ methodM POST >> postShoeAsJson conf
    , dir "item" $ path (\no -> showShoe conf no)
    , dir "static" $ serveDirectory DisableBrowsing [] (staticDir conf)
    , dir "img" $ serveDirectory DisableBrowsing [] (imgsDir conf)
    , seeOther homeFullPath (toResponse())
  ] where homeFullPath = "/" ++ home
