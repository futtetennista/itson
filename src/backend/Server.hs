{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import qualified Hack2 as H2 ( Application(..), Response(..), queryString
                             , headers, httpHeaders)
import qualified Hack2.Contrib.Response as H2R ( set_body_bytestring )
import qualified Hack2.Handler.SnapServer as Server ( run )
import Data.Default ( def )
import Network.HTTP.Types.URI ( decodePathSegments )
import qualified Data.Text as T ( Text(..), append, empty, splitOn, unpack
                                , pack, intercalate, null )
import qualified Data.ByteString.Char8 as C8 ( pack, unpack, split )
import qualified Data.ByteString as BS ( ByteString(..), empty )
--import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL ( toStrict )
import qualified Data.Aeson as Json
import Data.Char ( toLower )
import Data.Maybe ( fromMaybe )
import qualified Spotify
import qualified Utils.Geo as Geo


-- Incoming Request
getQueryParams env =
               let queryArray = map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments (H2.queryString env))
               in [ (a, b) | arr <- queryArray, a <- [ head arr ], b <- tail arr ]
getQueryParamValue p env =
                   let param = filter (\(a, b) -> a == p) (getQueryParams env)
                   in case param of
                     [] -> T.empty
                     _  -> snd $ head param

--printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
--requestHeadersToBS headers = BSL.toStrict $ toLazyByteString (printableRequestHeaders headers)

getIp headers =
  C8.unpack $ head (C8.split ':' (requestHeaderValue headers "Host"))

  where requestHeaderValue headers hk =
          case requestHeader headers hk of
            []       -> BS.empty
            [(_, v)] -> v

        requestHeader headers hk =
          let key = C8.pack hk
          in filter (\(k, _) -> k == key) headers


app :: H2.Application
app = \env ->
  let searchTerm = getQueryParamValue "term" env
  in if T.null searchTerm
     then return $ response . BSL.toStrict Json.encode (Error errorEmptySearchTerm)
     else
       do countryCode <- Geo.findCountryCode $ getIp headers
          fragment    <- Spotify.search defaultRequest{ term = searchTerm
                                                      , countryCode = countryCode
                                                      }
          return $ response . BLS.toStrict Json.encode (Results [ fragment ])

  where
    headers = H2.httpHeaders env

    response body =
      H2R.set_body_bytestring body (def { H2.headers = [ ("Content-Type", "application/json")
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
