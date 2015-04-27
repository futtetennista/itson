{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Spotify where

import qualified Data.Aeson as Json
import Data.Aeson.TH
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.List (stripPrefix,dropWhileEnd)
import Data.Char (toLower)

data APIAlbumsResponse =
  APIAlbumsResponse { albums :: Items Album } deriving Show

data APIArtistsResponse =
  APIArtistsResponse { artists :: Items Artist } deriving Show

data APITracksResponse =
  APITracksResponse { tracks :: Items Track } deriving Show

data Response = Artists { _items :: [Data'] }
              | Albums { _items :: [Data'] }
              | Tracks { _items :: [Data'] } deriving Show

data Items type' =
  Items { items :: [type'] } deriving Show

data Data'
  = Track' { name :: String
          , album :: Album
          , artists' :: [Artist]
          , preview_url' :: String
          , available_markets :: [String]
          , external_urls :: ExternalUrls
          }
  | Artist' { _images :: Maybe [Image]
           , _href :: String
           , _name :: String
           , _external_urls :: ExternalUrls
           }
  | Album' { available_markets :: [String]
          , album_type' :: String
          , images :: Maybe [Image]
          , href :: String
          , external_urls :: ExternalUrls
          , name :: String
          } deriving Show
-- TODO: is there a better way to name common fields instead of prefixing?!
data Track =
  Track { track__name :: String
        , track__album :: Album
        , track__artists :: [Artist]
        , preview_url :: String
        , track__available_markets :: [String]
        , track__external_urls :: ExternalUrls
        } deriving Show

data Artist =
  Artist { artist__images :: Maybe [Image]
         , artist__href :: String
         , artist__name :: String
         , artist__external_urls :: ExternalUrls
         } deriving Show

data Album =
  Album { album__available_markets :: [String]
        , album_type :: String
        , album__images :: [Image]
        , album__href :: String
        , album__external_urls :: ExternalUrls
        , album__name :: String
        } deriving Show

data Image =
  Image { height :: Int
        , url :: String
        , width :: Int
        } deriving Show

newtype ExternalUrls =
  ExternalUrls { spotify :: String } deriving Show


$(deriveJSON defaultOptions ''APITracksResponse)
$(deriveJSON defaultOptions ''APIAlbumsResponse)
$(deriveJSON defaultOptions ''APIArtistsResponse)
$(deriveJSON defaultOptions ''Items)
--TODO: extract that function, don't know enough about TemplateHaskell right now.
$(deriveJSON defaultOptions{fieldLabelModifier=(\field -> case stripPrefix "track__" field of
                                                               Just field' -> field'
                                                               Nothing -> field
                                               )} ''Track)
$(deriveJSON defaultOptions{fieldLabelModifier=(\field -> case stripPrefix "album__" field of
                                                               Just field' -> field'
                                                               Nothing -> field
                                               )} ''Album)
$(deriveJSON defaultOptions{fieldLabelModifier=(\field -> case stripPrefix "artist__" field of
                                                               Just field' -> field'
                                                               Nothing -> field
                                                )} ''Artist)
$(deriveJSON defaultOptions ''Image)
$(deriveJSON defaultOptions ''ExternalUrls)

$(deriveJSON defaultOptions{ fieldLabelModifier=drop 1
                           , constructorTagModifier=(map toLower) . dropWhileEnd (=='\'')
                           , sumEncoding=ObjectWithSingleField
                           } ''Response)
$(deriveJSON defaultOptions{ fieldLabelModifier=drop 1
                           , constructorTagModifier=(map toLower) . dropWhileEnd (=='\'')
                           , sumEncoding=defaultTaggedObject{ tagFieldName="type" }
                           } ''Data')


items' :: Items Album
items' =
  Items { items = [album'] }

album' :: Album
album' =
  Album { album__available_markets = []
        , album_type = "album_type"
        , album__images = []
        , album__external_urls = ExternalUrls { spotify = "asa" }
        , album__name = "name"
        , album__href = "href"
        }

main :: IO ()
main =
  do
    putStrLn "Test Album:"
    withFile "spotify_api_sample_album_response.json" ReadMode
      (\handle ->
        do
          contents <- L.hGetContents handle
          let m = Json.decode contents :: Maybe Response
          putStrLn $ show m
      )

    putStrLn "Test Tracks:"
    withFile "spotify_api_sample_track_response.json" ReadMode
      (\handle ->
        do
          contents <- L.hGetContents handle
          let m = Json.decode contents :: Maybe APITracksResponse
          putStrLn $ show m
      )
    putStrLn "Test Tracks:"
    withFile "spotify_api_sample_artist_response.json" ReadMode
      (\handle ->
        do
          contents <- L.hGetContents handle
          let m = Json.decode contents :: Maybe APIArtistsResponse
          putStrLn $ show m
      )
