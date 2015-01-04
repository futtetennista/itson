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
import Data.Text hiding (foldr, filter, head, tail, reverse, map)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, split)
import Data.Monoid
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL (toStrict)

-- TODO: all these appends are awful, find a better solution
buildJson xs = dropWhileEnd (==',') $ foldr (\e json -> append (append (append (pack "\"") e) (pack "\",")) json) empty xs
buildResponseBody r = append (append (pack "{\"results\":{\"previewUrls\":[") (buildJson $ extractPreviewUrls r)) (pack "]}}")
extractPreviewUrls r = r ^.. responseBody . key "tracks" . key "items" . values . key "preview_url" . _String

getQueryParams (Env _ _ _ queryString _ _ _ _ _ _ _ _) =
               let queryArray = map (splitOn "=") $ splitOn "&" (head $ decodePathSegments queryString)
               in [ (a, b) | arr <- queryArray, a <- [head arr], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)

extractRequestHeaders (Env _ _ _ _ _ _ httpHeaders _ _ _ _ _) = httpHeaders
printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
requestHeadersToBS headers = toStrict $ toLazyByteString (printableRequestHeaders headers)
getRequestHeader headers hk =
                 let key = BSC8.pack hk
                 in filter (\(k, v) -> k == key) headers
getRequestHeaderValue headers hk =
                 case getRequestHeader headers hk of
                      []       -> BS.empty
                      [(k, v)] -> v
getIpFromRequest headers = BSC8.unpack $ head (BSC8.split ':' (getRequestHeaderValue headers "Host"))
findRequestCountryCode headers =
                       do
                         r <- get $ "http://www.telize.com/geoip/" ++ getIpFromRequest headers
                         return $ r ^. responseBody . key "country_code" . _String

app :: Application
app = \env ->
  let opts = defaults & param "q"     .~ [ getQueryParamValue "q" env ]
                      & param "type"  .~ ["track"]
                      & param "limit" .~ ["5"]
  in do
       cc <- findRequestCountryCode $ extractRequestHeaders env
       r <- getWith opts "https://api.spotify.com/v1/search"
       return $ set_body_bytestring (encodeUtf8 $ buildResponseBody r) (def { headers = [ ("Content-Type", "text/json") ] })

main = run app
