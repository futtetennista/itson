{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Soundcloud (Soundcloud(..), search, empty) where

import qualified Data.Aeson as Json
import Data.Aeson.TH
import Models as ItsOn
import Data.Char (toLower)
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T (unpack, splitOn, lines)
import Data.Text (Text)
import Data.Text.IO (hGetContents)
import System.IO (IOMode(ReadMode), withFile)
-- import qualified Data.ByteString.Lazy as L
-- import Control.Concurrent.Async (async, waitCatch)


data Soundcloud = Soundcloud deriving Show

consumerKey :: IO (Either String String)
consumerKey =
  do withFile "config.properties" ReadMode
       (\handle ->
         do contents <- hGetContents handle
            case getConsumerKey contents of
              [key] -> return $ Right (T.unpack key)
              (_:_) -> return $ Left "Multiple values for Soundcloud consumer key"
              _     ->
                return $ Left "Soundcloud consumer key not found. \
\ Possible issues are: \n\
\ 1) config.properties file is missing \n\
\ 2) the configuration file doesn't have a soundcloud.consumer_key entry \n")

getConsumerKey :: Text -> [Text]
getConsumerKey properties =
  value . filterConsumerKey . toPair . toArray $ properties

  where value kvs =
          case kvs of
            (Just (_, v)):xs -> [v] ++ value xs
            _                -> []

        filterConsumerKey xs =
          filter (\x ->
                   case x of
                     Nothing     -> False
                     Just (k, _) ->
                       if (k == "soundcloud.consumer_key")
                       then True
                       else False) xs
        toPair arr =
          map (\xs ->
                case xs of
                  [k, v] -> Just (k, v)
                  _      -> Nothing) arr

        toArray properties =
          map (T.splitOn "=") (T.lines properties)

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
    do key <- consumerKey
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
