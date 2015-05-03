{-# LANGUAGE TemplateHaskell #-}
module Utils.Geo where

import Data.Aeson as Json (decode)
import Data.Aeson.TH
import qualified Network.HTTP.Client as HTTP

data Telize =
  Telize { country_code :: String }

$(deriveJSON defaultOptions ''Telize)

-- TODO: 1) define time-out 2) define exception handling
findCountryCode :: String -> IO String
findCountryCode ip =
  do httpRequest <- HTTP.parseUrl "http://www.telize.com/geoip/2.240.222.127" -- ++ ip
     -- for testing purposes use a static ip
     response  <- HTTP.withManager HTTP.defaultManagerSettings $ HTTP.httpLbs httpRequest
     let telize = Json.decode $ HTTP.responseBody response :: Maybe Telize
     case telize of
       Just t  -> return $ country_code t
       Nothing -> return ""
