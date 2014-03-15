{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Shoes.Domain.Model where

import Data.IxSet(IxSet)
import Data.Data(Data,Typeable)
import Data.SafeCopy(SafeCopy)

type ShoePhotoFileName = String

newtype ShoeId = ShoeId Integer
  deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Show ShoeId where
  show (ShoeId i) = show i

data PersistedShoeData = PersistedShoeData {
  shoeId :: Integer
  , shoeData :: ShoeData
} deriving (Eq, Ord, Show, Data, Typeable)

data ShoeData = ShoeData {
  description :: String
  , color :: String
  , size :: String
  , photoFileName :: ShoePhotoFileName
} deriving (Eq, Ord, Show, Data, Typeable)

data ShoeStorage = ShoeStorage {
    lastId :: Integer
  , persistedShoeData :: IxSet PersistedShoeData
} deriving (Eq, Ord, Show, Data, Typeable)
