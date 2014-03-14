{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Shoes.Storage where

import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.Data(Data,Typeable)
import Data.Acid(Query, Update, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy, SafeCopy)
import Data.IxSet(Indexable(..), IxSet, (@>), Proxy(..), ixFun, ixSet, toDescList, insert)

type ShoePhotoFileName = String

newtype ShoeId = ShoeId Integer
  deriving (Eq, Ord, Data, Typeable, SafeCopy, Show)

data ShoeData = ShoeData {
    shoeId :: Integer
  , description :: String
  , color :: String
  , size :: String
  , photoFileName :: ShoePhotoFileName
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeData)

data ShoeStorage = ShoeStorage {
    lastId :: Integer
  , shoeSet :: IxSet ShoeData
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [ShoeData]
fetchAll = do
  ShoeStorage{..} <- ask
  return $ toDescList (Proxy :: Proxy ShoeId) $ shoeSet @> (ShoeId 0)

insertShoe :: ShoeData -> Update ShoeStorage Integer
insertShoe shoe = do
  s <- get
  let newId = succ $ lastId s
  put $ s {
      lastId = newId
    , shoeSet = insert (shoe { shoeId = newId }) (shoeSet s)
  }
  return newId

$(makeAcidic ''ShoeStorage ['fetchAll, 'insertShoe])

instance Indexable ShoeData where
  empty = ixSet
    [ ixFun $ \bp -> [ ShoeId (shoeId bp) ] ]

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 empty