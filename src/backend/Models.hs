{-# LANGUAGE TemplateHaskell #-}

module Models where

import Data.Aeson.TH
import Data.Char (toLower)


data Request =
  Request { term        :: String
          , type'       :: Type
          , maxResults  :: Int
          , countryCode :: CountryCode
          }

defaultRequest :: Request
defaultRequest =
  Request { term        = ""
          , type'       = TTrack
          , maxResults  = 5
          , countryCode = ""
          }

data Response = Results [Fragment]
              | Error { message :: String }

data Fragment =
  Fragment { service :: String
           , items   :: [Item]
           } deriving Show

data Item = Track { title   :: String
                  , artists :: [String]
                  , album   :: String
                  , urls    :: Urls
                  }
          | Album { title   :: String
                  , artists :: [String]
                  , urls    :: Urls
                  }
          deriving Show

data Urls =
  Urls { full    :: Maybe String
       , preview :: String
       } deriving Show

data Type = TAlbum
          | TTrack
          deriving Show

type CountryCode = String


$(deriveJSON defaultOptions{ fieldLabelModifier = drop 1} ''Type)
$(deriveJSON defaultOptions ''Urls)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = defaultTaggedObject{ tagFieldName = "type" }
                           } ''Item)
$(deriveJSON defaultOptions ''Fragment)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = ObjectWithSingleField
                           } ''Response)
