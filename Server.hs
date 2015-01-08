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
import qualified Data.Text as T (dropWhileEnd, append, empty, splitOn)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import Data.Monoid
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Vector as V (filter, null, empty)

-- TODO: all these appends are awful, find a better solution
buildJson xs = T.dropWhileEnd (==',') $ foldr (\e json -> T.append (T.append (T.append "\"" e) ("\",")) json) T.empty xs
buildResponseBody r = T.append (T.append "{\"results\":{\"previewUrls\":[" (buildJson $ extractPreviewUrls r)) "]}}"
extractPreviewUrls r = r ^.. responseBody . key "tracks" . key "items" . values . key "preview_url" . _String
extractAvailableMarkets r =
                        let mx = r ^.. responseBody . key "tracks" . key "items" . values . key "available_markets" . _Array
                        in
                          case mx of
                               [] -> V.empty
                               xs -> head mx
                   
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
       r <- getWith opts "https://api.spotify.com/v1/search"
       if V.null $ V.filter (== AT.String cc) (extractAvailableMarkets r) then
         return $ set_body_bytestring (encodeUtf8 $ "results {}") (def { headers = [ ("Content-Type", "text/json") ] })
       else
         return $ set_body_bytestring (encodeUtf8 $ buildResponseBody r) (def { headers = [ ("Content-Type", "text/json") ] })

main = run app
