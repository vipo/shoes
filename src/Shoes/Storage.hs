{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Shoes.Storage where

import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.Data(Data,Typeable)
import Data.Acid(Query, Update, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy)
import Data.IxSet(Indexable(..), IxSet, (@=), Proxy(..), ixFun, ixSet, toDescList, insert)

type ShoeId = String
type ShoePhotoFileName = String

data ShoeData = ShoeData {
    shoeId :: ShoeId
  , description :: String
  , color :: String
  , size :: String
  , photoFileName :: ShoePhotoFileName
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeData)

data ShoeStorage = ShoeStorage {
    counter :: Integer
  , shoeSet :: IxSet ShoeData
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [ShoeData]
fetchAll = do
  ShoeStorage{..} <- ask
  return $ toDescList (Proxy :: Proxy String) $ shoeSet @= shoeId

insertShoe :: ShoeData -> Update ShoeStorage ShoeId
insertShoe shoe = do
  s <- get
  let newCounter = succ $ counter s
  let newShoeId = show newCounter
  put $ s {
      counter = newCounter
    , shoeSet = insert (shoe { shoeId = newShoeId}) (shoeSet s)
  }
  return newShoeId

$(makeAcidic ''ShoeStorage ['fetchAll, 'insertShoe])

instance Indexable ShoeData where
  empty = ixSet
    [ ixFun $ \bp -> [ shoeId bp ] ]

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 empty