{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts,
  GeneralizedNewtypeDeriving, MultiParamTypeClasses,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Shoes.Domain.Data where

import Control.Applicative((<$>))
import Control.Monad.Reader(ask)
import Control.Monad.State(get, put)
import Data.Data(Data,Typeable)
import Data.Acid(AcidState, Query, Update, makeAcidic, openLocalState)
import Data.Acid.Advanced(query', update')
import Data.SafeCopy(base, deriveSafeCopy)
import Data.IxSet(Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, toList, toDescList)

data ShoeData = ShoeData {
  shoeId :: String
  , description :: String
  , color :: String
  , size :: Integer
  , photoFileName :: String
} deriving (Eq, Ord, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeData)

data ShoeStorage = ShoeStorage {
  seq :: Int
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