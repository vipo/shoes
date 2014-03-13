{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Shoes.Controller(postShoeAsJson, listShoes) where

import Happstack.Server (ok, badRequest)
import Happstack.Server.SimpleHTTP (ServerPart, askRq, takeRequestBody, unBody)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as BL
import Data.Acid.Advanced(query', update')
import Data.Aeson
import Data.UUID.V4(nextRandom)
import Data.UUID(toString)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, mzero)
import Control.Applicative ((<*>), (<$>))

import Shoes.Storage(ShoeData(ShoeData), ShoeId, ShoePhotoFileName, InsertShoe(..))
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

postShoeAsJson :: AppConf -> ServerPart String
postShoeAsJson conf = do
  bytes <- getBodyBytes
  let jsonObject = decode bytes :: Maybe JsonRequest
  case jsonObject of
  	Nothing -> badRequest "Could not parse json request"
  	Just o -> case (photo o) of
  	  Left msg -> badRequest $ "Bad photo encoding: " ++ msg
  	  Right bytes -> storeData conf (newShoe (description o) (color o) (size o)) bytes

storeData :: AppConf -> (ShoePhotoFileName -> ShoeData) -> B.ByteString -> ServerPart String
storeData conf shoeData photoBytes = do
  uuid <- liftIO nextRandom
  let photoFileName = (toString uuid) ++ ".jpg"
  let photoFullPath = (imgsDir conf) ++ photoFileName
  liftIO $ B.writeFile photoFullPath photoBytes
  showId <- update' (acidState conf) $ InsertShoe (shoeData photoFileName)
  ok showId

getBodyBytes :: ServerPart L.ByteString
getBodyBytes = do
  req  <- askRq 
  body <- liftIO $ takeRequestBody req 
  case body of 
    Just rqbody -> return . unBody $ rqbody 
    Nothing -> return L.empty

newShoe :: String -> String -> String -> ShoePhotoFileName -> ShoeData
newShoe = ShoeData "" -- this fake id will be replaced by an unique one on persistence