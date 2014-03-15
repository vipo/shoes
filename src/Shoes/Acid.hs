{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Shoes.Acid where

import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.Data(Data,Typeable)
import Data.Acid(Query, Update, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy, SafeCopy)
import Data.IxSet(Indexable(..), IxSet, (@>), (@=), Proxy(..), ixFun, ixSet, getOne, toDescList, insert)

import Shoes.Domain.Model

$(deriveSafeCopy 0 'base ''ShoeData)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [ShoeData]
fetchAll = do
  ShoeStorage{..} <- ask
  return $ toDescList (Proxy :: Proxy ShoeId) $ shoeSet @> (ShoeId 0)

fetchOne :: ShoeId -> Query ShoeStorage (Maybe ShoeData)
fetchOne shoeId = do
  ShoeStorage{..} <- ask
  return $ getOne $ shoeSet @= shoeId

insertShoe :: ShoeData -> Update ShoeStorage Integer
insertShoe shoe = do
  s <- get
  let newId = succ $ lastId s
  put $ s {
      lastId = newId
    , shoeSet = insert (shoe { shoeId = newId }) (shoeSet s)
  }
  return newId

$(makeAcidic ''ShoeStorage ['fetchAll, 'fetchOne, 'insertShoe])

instance Indexable ShoeData where
  empty = ixSet
    [ ixFun $ \bp -> [ ShoeId (shoeId bp) ] ]

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 empty
