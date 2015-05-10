{-# LANGUAGE ExistentialQuantification #-}
module Service where

import Models (Request, Fragment)

class Service a where
  search :: a -> Request -> IO Fragment
  empty  :: a -> Fragment

data ServiceWrapper = forall s . Service s => MkServiceWrapper s

wrap :: Service s => s -> ServiceWrapper
wrap = MkServiceWrapper
