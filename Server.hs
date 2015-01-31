{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import qualified Hack2 as H2
import qualified Hack2.Contrib.Response as H2R (set_body_bytestring)
import Hack2.Handler.SnapServer
import Data.Default (def)
-- http://www.serpentine.com/wreq/
import Network.Wreq as Wreq hiding (headers)
import Network.HTTP.Types.URI (decodePathSegments)
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Aeson.Types as AT (Value(String), emptyArray)
import qualified Data.Text as T (Text(..), dropWhileEnd, append, empty, splitOn, unpack, pack, intercalate)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import Data.Monoid
import qualified Data.ByteString as BS (ByteString(..), empty, unpack)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Vector as V (filter, null, empty)
import qualified Data.Aeson as A
import Data.Char (toLower)

-- Models
data Service = Spotify deriving Show
dataProviders = [Spotify]

data Urls  = U [(String, String)] deriving Show
data Title = TT String deriving Show
data Item = I Title Urls | Empty deriving Show
data ServiceData = SD { name  :: String
                      , items :: [Item]
                      } deriving Show
data Body = B [ServiceData] deriving Show

instance A.ToJSON Item where
         toJSON (I (TT title) (U kvs)) = A.object $ ("title" A..= title) : ["urls" A..= A.object (map (\(k, v) -> T.pack k A..= v) kvs)]
instance A.ToJSON ServiceData where
         toJSON (SD name [])    = A.object ["service" A..= name, "items" A..= AT.emptyArray]
         toJSON (SD name items) = A.object ["service" A..= name, "items" A..= items]
instance A.ToJSON Body where
         toJSON (B body) = A.object ["results" A..= body]

-- Serialisation
intercalateM sep xs = T.intercalate sep . filter (/="") $ map (\ m -> case m of { Just v -> v; Nothing -> "" }) xs
--createTitle :: IsString s => Maybe s -> [Maybe s] -> Title 
createTitle (Just t) (a:as) = TT $ (T.unpack t) ++ " by " ++ (T.unpack $ intercalateM ", " (a:as))
createTitle (Just t) []     = TT (T.unpack t)
createTitle Nothing  _      = TT "Song Details Unknown"
--createUrls :: Data.String.IsString s => Maybe s -> Maybe s -> Urls
createUrls (Just p) (Just f) = U [("preview", T.unpack p), ("full", T.unpack f)]
createUrls Nothing  (Just f) = U [("full", T.unpack f)]
createUrls _ _               = U []

createSpotifyRequest query =
                     (opts, "https://api.spotify.com/v1/search")
                     where
                          opts = defaults & param "q"     .~ [query]
                                          & param "type"  .~ ["track"]
                                          & param "limit" .~ ["5"]
-- Parsing
parseAvailableMarkets r =
                      let mx = r ^.. responseBody . key "tracks" . key "items" . values . key "available_markets" . _Array
                      in case mx of
                              [] -> V.empty
                              xs -> head mx
parseData :: (AsValue body) => Service -> Wreq.Response body -> [Item]
parseData service resp = resp ^.. responseBody . key "tracks" . key "items" . _Array . traverse . to (\o -> I (createTitle (o ^? key "name" . _String) (o ^.. key "artists" . _Array . traverse . to (\a -> a ^? key "name" . _String))) (createUrls (o ^? key "preview_url" . _String) (o ^? key "external_urls" . key "spotify" . _String)))

-- Incoming Request
getQueryParams env =
               let queryArray = map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments (H2.queryString env))
               in [ (a, b) | arr <- queryArray, a <- [head arr], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)

printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
requestHeadersToBS headers = BSL.toStrict $ toLazyByteString (printableRequestHeaders headers)
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

buildServiceData :: Service -> [Item] -> ServiceData
buildServiceData service datas =
                 SD { name  = map toLower $ show service
                    , items = datas
                    }

--search :: T.Text -> [ServiceData]
--search query = map getServiceData $ zip dataProviders [query]

--bodyToJSON :: (Body b, Data.ByteString.Lazy.Internal.ByteString lbs) => b -> lbs
bodyToJSON b = A.encode b
createResponse :: Body -> BS.ByteString
createResponse res = BSL.toStrict $ bodyToJSON res

app :: H2.Application
app = \env ->
    let q = getQueryParamValue "q" env
        urlComponents = createSpotifyRequest q
    in do
        --putStrLn $ "Searching results for term: \"" ++ T.unpack q ++ "\""
        --countryCode <- findRequestCountryCode $ H2.httpHeaders env
        r <- getWith (fst urlComponents) (snd urlComponents)
        --putStrLn "got response from spotify. Parsing..."
        let results = parseData Spotify r
        --putStrLn "parsing done...creating response"
        let searchResults = B $ [buildServiceData Spotify results]
        let body = createResponse $ searchResults
        --putStrLn $ C8.unpack body
        --let searchResults = search $ getQueryParamValue "q" env
        return $ H2R.set_body_bytestring body (def { H2.headers = [ ("Content-Type", "text/json") ] })

main = run app
