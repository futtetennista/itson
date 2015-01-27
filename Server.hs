{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import Hack2.Handler.SnapServer
import Data.Default (def)
-- http://www.serpentine.com/wreq/
import Network.Wreq hiding (headers)
import Network.HTTP.Types.URI (decodePathSegments)
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Aeson.Types as AT (Value(String))
import qualified Data.Text as T (dropWhileEnd, append, empty, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import Data.Monoid
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Vector as V (filter, null, empty)
import qualified Data.Aeson as A

dataProviders = ["spotify"]

-- Models
data Detail = T String | URLS [String] deriving Show
data Item = I Detail Detail deriving Show
data ServiceData = SD { name  :: String
                      , items :: [Item]
                      } deriving Show
data Body = B [ServiceData] deriving Show

instance A.ToJSON Detail where
         toJSON (T title) = A.object ["title" A..= title]
         toJSON (URLS xs) = A.object ["previewUrls" A..= xs]
instance A.ToJSON Item where
         toJSON (I (T title) (URLS urls)) = A.object ["title" A..= title, "previewUrls" A..= urls]
instance A.ToJSON ServiceData where
         toJSON (SD name items) = A.object ["service" A..= name, "items" A..= items]
instance A.ToJSON Body where
         toJSON (B body) = A.object ["results" A..= body]

-- Serialisation
concatStrings (sep, xs) =
              let s = (foldr (\e res -> "\"" ++ e ++ "\"" ++ sep ++ res) "" xs)
              in take (length s - 1) s
createTitle :: (String, [String]) -> String
createTitle (title, artists)
            | (not $ null artists) && (not $ null title) = title ++ " by " ++ concatStrings (",", artists)
            | null artists = title
            | otherwise    = "Song Details Unknown"
-- buildItem r = I (T $ createTitle (parseTitles r, parseArtists r)) (URLS $ parsePreviewUrls r)
-- buildServiceData :: String ->  -> ServiceData
buildServiceData service d =
                 SD { name  = service
                    , items = [I (T "") (T "")]
                    }
getServiceData :: (String, String) -> ServiceData
getServiceData (service, query) =
               let opts = defaults & param "q"     .~ [query]
                                   & param "type"  .~ ["track"]
                                   & param "limit" .~ ["5"]
               in do
                  r       <- getWith opts "https://api.spotify.com/v1/search"
                  results <- parseData r
                  return $ buildServiceData service results
                  
search :: String -> [ServiceData]               
search query = map getServiceData $ zip dataProviders [query]
buildBody :: [ServiceData] -> Body
buildBody datas = B datas
-- bodyToJSON :: Body b, Data.ByteString bs => b -> bs
bodyToJSON b = A.encode b
-- bodyToString $ buildBody (buildServiceData "Spotify" r)

-- Parsing
parseAvailableMarkets r =
                      let mx = r ^.. responseBody . key "tracks" . key "items" . values . key "available_markets" . _Array
                      in case mx of
                              [] -> V.empty
                              xs -> head mx

parseData r = r ^.. responseBody . key "tracks" . key "items" . _Array . traverse . to (\o -> (o ^? key "name" . _String, o ^? key "preview_url" . _String, o ^? key "external_urls" .key "spotify" . _String, o ^.. key "artists" . _Array . traverse . to (\a -> a ^? key "name" . _String)))

-- Request
getQueryParams (Env _ _ _ queryString _ _ _ _ _ _ _ _) =
               let queryArray = map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments queryString)
               in [ (a, b) | arr <- queryArray, a <- [head arr], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)

extractRequestHeaders (Env _ _ _ _ _ _ httpHeaders _ _ _ _ _) = httpHeaders
printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
requestHeadersToBS headers = toStrict $ toLazyByteString (printableRequestHeaders headers)
getRequestHeader headers hk =
                 let key = C8.pack hk
                 in filter (\(k, v) -> k == key) headers
getRequestHeaderValue headers hk =
                 case getRequestHeader headers hk of
                      []       -> BS.empty
                      [(k, v)] -> v
getIpFromRequest headers = C8.unpack $ head (C8.split ':' (getRequestHeaderValue headers "Host"))
findRequestCountryCode headers =
                       do
                         -- for testing purposes use a static ip
                         r <- get $ "http://www.telize.com/geoip/2.240.222.127"
                         -- r <- get $ "http://www.telize.com/geoip/" ++ getIpFromRequest headers
                         return $ r ^. responseBody . key "country_code" . _String

app :: Application
app = \env ->
  let opts = defaults & param "q"     .~ [ getQueryParamValue "q" env ]
                      & param "type"  .~ ["track"]
                      & param "limit" .~ ["5"]
  in do
       cc <- findRequestCountryCode $ extractRequestHeaders env
       searchResults  <- search $ getQueryParamValue "q" env
       if V.null $ V.filter (== AT.String cc) (parseAvailableMarkets r) then
         return $ set_body_bytestring (encodeUtf8 "results {}") (def { headers = [ ("Content-Type", "text/json") ] })
       else
         return $ set_body_bytestring (encodeUtf8 "") (def { headers = [ ("Content-Type", "text/json") ] })

main = run app
