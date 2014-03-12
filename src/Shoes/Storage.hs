{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Shoes.Storage where

import Control.Monad.Reader(ask)
import Data.Data(Data,Typeable)
import Data.Acid(Query, makeAcidic)
import Data.SafeCopy(base, deriveSafeCopy)
import Data.IxSet(Indexable(..), IxSet, (@=), Proxy(..), ixFun, ixSet, toDescList)

data ShoeData = ShoeData {
  shoeId :: String
  , description :: String
  , color :: String
  , size :: Integer
  , photoFileName :: String
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeData)

data ShoeStorage = ShoeStorage {
  counter :: Int
  , shoeSet :: IxSet ShoeData
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [ShoeData]
fetchAll = do
  ShoeStorage{..} <- ask
  return $ toDescList (Proxy :: Proxy String) $ shoeSet @= shoeId

$(makeAcidic ''ShoeStorage ['fetchAll])

instance Indexable ShoeData where
  empty = ixSet
    [ ixFun $ \bp -> [ shoeId bp ] ]

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 empty