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

data ShoeData = ShoeData {
  id :: String
  , description :: String
  , color :: String
  , size :: Integer
  , photoFileName :: String	
} deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeData)

data ShoeStorage = ShoeStorage {
  seq :: Int
  , shoeSet :: [ShoeData]
} deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ShoeStorage)

fetchAll :: Query ShoeStorage [ShoeData]
fetchAll = shoeSet <$> ask

$(makeAcidic ''ShoeStorage ['fetchAll])

initialDataState :: ShoeStorage
initialDataState = ShoeStorage 0 []