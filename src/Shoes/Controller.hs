module Shoes.Controller(postShoeAsJson, listShoes) where

import Happstack.Server (ok)
import Happstack.Server.SimpleHTTP (ServerPart, askRq, takeRequestBody, unBody)
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)

postShoeAsJson :: ServerPart String
postShoeAsJson = getBodyBytes >> ok "OK"

listShoes :: ServerPart String
listShoes = ok "LISt"

getBodyBytes :: ServerPart L.ByteString
getBodyBytes = do
  req  <- askRq 
  body <- liftIO $ takeRequestBody req 
  case body of 
    Just rqbody -> return . unBody $ rqbody 
    Nothing -> return L.empty