{-# LANGUAGE FlexibleContexts #-}
module Shoes.Router where

import Happstack.Server (ok)
import Happstack.Server.Types (Response)
import Happstack.Server.Monads (FilterMonad)
import Happstack.Server.SimpleHTTP (ServerPart, ToMessage)

import Control.Monad (msum, MonadPlus, mzero, liftM)

router :: ServerPart String
router = msum [
    ok "Hello, World!"
  ]