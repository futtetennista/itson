{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Soundcloud (Soundcloud(..), search, empty) where

import qualified Data.Aeson as Json
import Data.Aeson.TH
import Models as ItsOn
import Data.Char (toLower)
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Utils.Properties as Properties
-- import qualified Data.ByteString.Lazy as L
-- import Control.Concurrent.Async (async, waitCatch)


data Soundcloud = Soundcloud deriving Show

-- SOUNDCLOUD MODELS
data Response = Tracks [Model] deriving Show

data Model = Track { title :: String
                   , user  :: User
                   , uri   :: String
                   }
           | NotYetImplemented deriving Show

newtype User =
  User { username :: String } deriving Show

-- JSON
$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = defaultTaggedObject{ tagFieldName = "kind"}
                           } ''Model)
$(deriveJSON defaultOptions ''Soundcloud.Response)

-- IT'S ON MODELS
emptyFragment :: Fragment
emptyFragment =
  Fragment { service       = "Soundcloud"
           , ItsOn.items   = []
           }

toFragment :: CountryCode -> Soundcloud.Response -> Fragment
toFragment market (Tracks models) =
  emptyFragment { items = map (toTrack market) models }

toTrack :: CountryCode -> Model -> Item
toTrack _ model =
  ItsOn.Track { ItsOn.title = Soundcloud.title model
              , artists     = map username [Soundcloud.user model]
              , album       = "" -- no info about it
              , urls        = Urls { full    = Just $ uri model
                                   , preview = Nothing
                                   }
              }

instance Service Soundcloud where
  empty Soundcloud = emptyFragment

  search Soundcloud request =
    do key <- Properties.getProperty "soundcloud.consumer_key"
       case key of
         Left _ -> return emptyFragment

         Right k  ->
           do httpRequest <- toHTTPRequest request k
              response <- HTTP.withManager tlsManagerSettings $ HTTP.httpLbs httpRequest
              let result = Json.decode $ HTTP.responseBody response :: Maybe Soundcloud.Response
              case result of
               Just r  -> return $ toFragment (countryCode request) r
               Nothing -> return emptyFragment

    where toHTTPRequest req key =
            do initRequest <- HTTP.parseUrl "https://api.soundcloud.com/tracks.json"
               let request = initRequest{ HTTP.requestHeaders = headers }
               return $ HTTP.setQueryString [ (pack "type", packMaybe "tracks")
                                            , (pack "limit", packMaybe (show $ numResults req))
                                            , (pack "q", packMaybe (term req))
                                            , (pack "consumer_key", packMaybe key)
                                            , (pack "order", packMaybe "created_at")
                                            ] request

            where headers   = [ ("Content-Type", "application/json") ]
                  packMaybe = Just . pack


-- main :: IO ()
-- main =
--   do
--     searchAsync <- async $ search Soundcloud defaultRequest{ term = "123" }
--     fragment <- waitCatch searchAsync
--     res <- case fragment of
--              Left _   -> return $ show "Kaputt"
--              Right f  -> return $ show f
--     print res
--     putStrLn "Parse tracks test:"
     -- withFile "test_data/soundcloud_api_sample_track_response.json" ReadMode
     -- (\handle ->
     --   do
     --     contents <- L.hGetContents handle
     --     let m = Json.decode contents :: Maybe Soundcloud.Response
     --     putStrLn $ show m)
