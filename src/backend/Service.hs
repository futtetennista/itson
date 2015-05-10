{-# LANGUAGE MultiParamTypeClasses #-}
module Service where

import Models (Request, Fragment)

class Service a where
  search :: a -> Request -> IO Fragment
  empty  :: a -> Fragment
