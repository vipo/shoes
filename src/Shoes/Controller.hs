{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Shoes.Controller(postShoeAsJson, listShoes) where

import Happstack.Server (ok, badRequest)
import Happstack.Server.SimpleHTTP (ServerPart, askRq, takeRequestBody, unBody)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as BL
import Data.Aeson
import Data.UUID.V4(nextRandom)
import Data.UUID(toString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, mzero)
import Control.Applicative ((<*>), (<$>))

import Shoes.Storage(ShoeData(ShoeData), ShoeId, ShoePhotoFileName)
import Shoes.Environment

data JsonRequest = JsonRequest {
    description :: String
  , color :: String
  , size :: String
  , photo :: Either String B.ByteString
} deriving (Show)

instance FromJSON JsonRequest where
  parseJSON (Object v) = JsonRequest <$>
    (v .: "description") <*>
    (v .: "color") <*>
    (v .: "size") <*>
    liftM BL.decode (v .: "photo")
  parseJSON _ = mzero

listShoes :: ServerPart String
listShoes = ok "LISt"

postShoeAsJson :: ServerPart String
postShoeAsJson = do
  bytes <- getBodyBytes
  let jsonObject = decode bytes :: Maybe JsonRequest
  case jsonObject of
  	Nothing -> badRequest "Could not parse json request"
  	Just o -> case (photo o) of
  	  Left msg -> badRequest $ "Bad photo encoding: " ++ msg
  	  Right bytes -> storeData (ShoeData (description o) (color o) (size o)) bytes

storeData :: (ShoePhotoFileName -> ShoeId -> ShoeData) -> B.ByteString -> ServerPart String
storeData shoeData photoBytes = do
  uuid <- liftIO nextRandom
  let photoFileName = (toString uuid) ++ ".jpg"
  --B.writeFile () photoBytes
  ok photoFileName

getBodyBytes :: ServerPart L.ByteString
getBodyBytes = do
  req  <- askRq 
  body <- liftIO $ takeRequestBody req 
  case body of 
    Just rqbody -> return . unBody $ rqbody 
    Nothing -> return L.empty