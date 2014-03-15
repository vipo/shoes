{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
  RecordWildCards, TemplateHaskell, TypeFamilies,
  OverloadedStrings #-}

module Shoes.Domain.Model where

import Data.IxSet(IxSet)
import Data.Data(Data,Typeable)
import Data.SafeCopy(SafeCopy)

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

data ShoeStorage = ShoeStorage {
    lastId :: Integer
  , shoeSet :: IxSet ShoeData
} deriving (Eq, Ord, Show, Data, Typeable)
