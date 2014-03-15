{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shoes.Acid where

import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.Acid(Query, Update, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy)
import Data.IxSet(Indexable(..), (@>), (@=), Proxy(..), ixFun, ixSet, getOne, toDescList, insert)

import Shoes.Domain.Model

$(deriveSafeCopy 0 'base ''ShoeData)
$(deriveSafeCopy 0 'base ''PersistedShoeData)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [PersistedShoeData]
fetchAll = do
  ShoeStorage{..} <- ask
  return $ toDescList (Proxy :: Proxy ShoeId) $ persistedShoeData @> (ShoeId 0)

fetchOne :: ShoeId -> Query ShoeStorage (Maybe PersistedShoeData)
fetchOne shoeId = do
  ShoeStorage{..} <- ask
  return $ getOne $ persistedShoeData @= shoeId

insertShoe :: ShoeData -> Update ShoeStorage ShoeId
insertShoe shoe = do
  s <- get
  let newId = succ $ lastId s
  put $ s {
      lastId = newId
    , persistedShoeData = insert (PersistedShoeData newId shoe) (persistedShoeData s)
  }
  return $ ShoeId newId

$(makeAcidic ''ShoeStorage ['fetchAll, 'fetchOne, 'insertShoe])

instance Indexable PersistedShoeData where
  empty = ixSet
    [ ixFun $ \bp -> [ ShoeId (shoeId bp) ] ]

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 empty
