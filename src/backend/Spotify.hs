{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Spotify (search) where

import qualified Data.Aeson as Json
import Data.Aeson.TH
-- import System.IO
-- import qualified Data.ByteString.Lazy as L
import Data.Char (toLower)
import Data.ByteString.Char8 (pack)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Models as ItsOn


-- SPOTIFY MODELS
data Response = Artists { items :: [Model] }
              | Albums  { items :: [Model] }
              | Tracks  { items :: [Model] }
              deriving Show

data Model = Track { name              :: String
                   , album             :: Model
                   , artists           :: [Model]
                   , preview_url       :: String
                   , available_markets :: [String]
                   , external_urls     :: ExternalUrls
                   }
           | Artist { images        :: Maybe [Image]
                    , href          :: String
                    , name          :: String
                    , external_urls :: ExternalUrls
                    }
           | Album { available_markets :: [String]
                   , album_type        :: String
                   , images            :: Maybe [Image]
                   , href              :: String
                   , external_urls     :: ExternalUrls
                   , name              :: String
                   }
           deriving Show

data Image =
  Image { height :: Int
        , url    :: String
        , width  :: Int
        } deriving Show

newtype ExternalUrls =
  ExternalUrls { spotify :: String } deriving Show

-- JSON
$(deriveJSON defaultOptions ''Image)
$(deriveJSON defaultOptions ''ExternalUrls)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = ObjectWithSingleField
                           } ''Spotify.Response)
$(deriveJSON defaultOptions{ constructorTagModifier = map toLower
                           , sumEncoding = defaultTaggedObject{ tagFieldName = "type" }
                           } ''Model)

-- IT'S ON MODELS
emptyFragment :: Fragment
emptyFragment =
  Fragment { service       = "Spotify"
           , ItsOn.items   = []
           }

toFragment :: CountryCode -> Spotify.Response -> Fragment
toFragment market response =
  case response of
    Tracks r ->
      emptyFragment { ItsOn.items = map (toTrack market) (Spotify.items response) }

    _        -> emptyFragment

toTrack :: CountryCode -> Model -> Item
toTrack market model =
  ItsOn.Track { title = name model
              , ItsOn.artists = map name (Spotify.artists model)
              , ItsOn.album = name $ Spotify.album model
              , urls = Urls { full = getFullUrl model market
                            , preview = preview_url model
                            }
              }

  where getFullUrl model market =
          case isAvailable model market of
            True  -> Just $ spotify (external_urls model)
            False -> Nothing

        isAvailable model market =
          market `elem` available_markets model

-- HTTP
search :: ItsOn.Request -> IO Fragment
search request =
  do httpRequest <- toHTTPRequest request
     response <- HTTP.withManager tlsManagerSettings $ HTTP.httpLbs httpRequest
     let result = Json.decode $ HTTP.responseBody response :: Maybe Spotify.Response
     case result of
       Just r  -> return $ toFragment (countryCode request) r
       Nothing -> return emptyFragment

  where toHTTPRequest r =
          do initReq <- HTTP.parseUrl "https://api.spotify.com/v1/search"
             let req = initReq { HTTP.requestHeaders = headers }
             return $ HTTP.setQueryString [ (pack "type", Just $ pack (getType r))
                                          , (pack "limit", Just $ pack (show $ maxResults r))
                                          , (pack "q", Just $ pack (term r))
                                          ] req

        headers = [ ("Content-Type", "application/json") ]

        getType request =
          case type' request of
            TTrack -> "track"
            TAlbum -> "album"

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
