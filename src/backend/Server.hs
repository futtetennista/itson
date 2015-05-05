{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import qualified Hack2 as H2 (Application, Env, Response(..), queryString, headers, httpHeaders)
import qualified Hack2.Contrib.Response as H2R (set_body_bytestring)
import qualified Hack2.Handler.SnapServer as Server (run)
import Data.Default (def)
import Network.HTTP.Types.URI (decodePathSegments)
import qualified Data.Text as T (splitOn, unpack)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import qualified Data.ByteString as BS (empty, null)
import Data.ByteString (ByteString)
import Data.Text.Internal (Text)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Aeson as Json
import qualified Spotify
import qualified Utils.Geo as Geo
import Models


-- Incoming Request
getParams :: H2.Env -> [(Text, Text)]
getParams env =
  let query = H2.queryString env
  in if BS.null query
     then []
     else [ (a, b) | arr <- (queryArray query), a <- [ head arr ], b <- tail arr ]

  where queryArray query =
          map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments query)

getParamValue :: String -> H2.Env -> Maybe Data.Text.Internal.Text
getParamValue paramName env =
  let param = filter (\(a, _) -> T.unpack a == paramName) (getParams env)
  in case param of
       [] -> Nothing
       _  -> Just (snd $ head param)

--printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
--requestHeadersToBS headers = BSL.toStrict $ toLazyByteString (printableRequestHeaders headers)

getIp :: [(ByteString, ByteString)] -> String
getIp headers =
  C8.unpack $ head (C8.split ':' (requestHeaderValue headers "Host"))

  where requestHeaderValue headers hk =
          case requestHeader headers hk of
            [(_, v)] -> v
            _        -> BS.empty

        requestHeader headers hk =
          let key = C8.pack hk
          in filter (\(k, _) -> k == key) headers


app :: H2.Application
app = \env ->
  let searchTerm = getParamValue "term" env
  in case searchTerm of
       Nothing ->
         return $ response . BSL.toStrict $ Json.encode (Error errorEmptySearchTerm)

       Just t  ->
         do countryCode <- Geo.findCountryCode $ getIp (headers env)
            fragment    <- Spotify.search defaultRequest{ term = T.unpack t
                                                        , countryCode = countryCode
                                                        }
            return $ response . BSL.toStrict $ Json.encode (Results [ fragment ])

  where
    headers env = H2.httpHeaders env

    response body =
      H2R.set_body_bytestring body (def { H2.headers = [ ("Content-Type", "application/json ; charset=utf-8")
                                                       , ("Access-Control-Allow-Origin", "*")
                                                       ]
                                        })

    errorEmptySearchTerm = "No search term supplied. Include a \'term\' query parameter"


main = Server.run app

{-|
Spotify Tests:
* /?term=Damballah -> the last result is not available in Germany
* /?term=glück -> utf-8 encoding
* /?term=glück123 -> empty results
-}
