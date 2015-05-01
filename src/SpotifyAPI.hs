{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Spotify (search) where

import qualified Data.Aeson as Json
import Data.Aeson.TH
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.List (stripPrefix,dropWhileEnd)
import Data.Char (toLower)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS
import Data.ByteString.Char8 (pack)


data Response = Artists { items :: [Model] }
              | Albums  { items :: [Model] }
              | Tracks  { items :: [Model] }
              | Error   { message :: String } deriving Show

data Model
  = Track { name :: String
          , album :: Model
          , artists :: [Model]
          , preview_url :: String
          , available_markets :: [String]
          , external_urls :: ExternalUrls
          }
  | Artist { images :: Maybe [Image]
           , href :: String
           , name :: String
           , external_urls :: ExternalUrls
           }
  | Album { available_markets :: [String]
          , album_type :: String
          , images :: Maybe [Image]
          , href :: String
          , external_urls :: ExternalUrls
          , name :: String
          } deriving Show

data Image =
  Image { height :: Int
        , url :: String
        , width :: Int
        } deriving Show

newtype ExternalUrls =
  ExternalUrls { spotify :: String } deriving Show


$(deriveJSON defaultOptions ''Image)
$(deriveJSON defaultOptions ''ExternalUrls)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = ObjectWithSingleField
                           } ''Response)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = defaultTaggedObject{ tagFieldName = "type" }
                           } ''Model)


-- toSearchTrackRequest :: exceptions-0.8.0.2:Control.Monad.Catch.MonadThrow m =>
--                         [Char] -> m HTTP.Request
toSearchTrackRequest term =
  do initReq <- HTTP.parseUrl "https://api.spotify.com/v1/search"
     let req = initReq { HTTP.queryString = pack $ "type=track&limit=5&q=" ++ term
                       , HTTP.requestHeaders = [("Content-Type", "application/json")]
                       }
     return req

search :: String -> IO Response
search term =
  do request  <- toSearchTrackRequest term
     response <- HTTP.withManager tlsManagerSettings $ HTTP.httpLbs request
     let result = Json.decode $ HTTP.responseBody response :: Maybe Response
     case result of
       Just r  -> return r
       Nothing -> return Error { message ="Boooom" }

-- main :: IO ()
-- main =
--   do
--     putStrLn "Parse album test:"
--     withFile "spotify_api_sample_album_response.json" ReadMode
--       (\handle ->
--         do
--           contents <- L.hGetContents handle
--           let m = Json.decode contents :: Maybe Response
--           putStrLn $ show m
--       )

--     putStrLn "Parse tracks test:"
--     withFile "spotify_api_sample_track_response.json" ReadMode
--       (\handle ->
--         do
--           contents <- L.hGetContents handle
--           let m = Json.decode contents :: Maybe Response
--           putStrLn $ show m
--       )
--     putStrLn "Parse artists test:"
--     withFile "spotify_api_sample_artist_response.json" ReadMode
--       (\handle ->
--         do
--           contents <- L.hGetContents handle
--           let m = Json.decode contents :: Maybe Response
--           putStrLn $ show m
--       )
