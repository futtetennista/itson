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
import qualified Data.Aeson.Types as AT (Value(String))
import qualified Data.Text as T (dropWhileEnd, append, empty, splitOn, unpack, pack, intercalate)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import Data.Monoid
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Vector as V (filter, null, empty)
import qualified Data.Aeson as A
import Data.Char (toLower)
import Data.String

data Service = Spotify deriving Show
dataProviders = [Spotify]

-- Models
data Urls  = U [(String, String)] deriving Show
data Title = TT String deriving Show
data Item = I Title Urls deriving Show
data ServiceData = SD { name  :: String
                      , items :: [Item]
                      } deriving Show
data Body = B [ServiceData] deriving Show

instance A.ToJSON Item where
         toJSON (I (TT title) (U [(k, v)])) = A.object ["title" A..= title, (T.pack k) A..= v]
instance A.ToJSON ServiceData where
         toJSON (SD name items) = A.object ["service" A..= name, "items" A..= items]
instance A.ToJSON Body where
         toJSON (B body) = A.object ["results" A..= body]

-- Serialisation
intercalateM sep xs = T.intercalate sep . filter (/="") $ map (\ m -> case m of { Just v -> v; Nothing -> "" }) xs
--createTitle :: IsString s => Maybe s -> [Maybe s] -> Title 
createTitle (Just t) (a:as) = TT $ (T.unpack t) ++ " by " ++ (T.unpack $ intercalateM ", " (a:as))
createTitle (Just t) []     = TT (T.unpack t)
createTitle Nothing  _      = TT "Song Details Unknown"
--createUrls :: IsString s => Maybe s -> Maybe s -> Urls
createUrls (Just p) (Just f) = U [("preview", T.unpack p), ("full", T.unpack f)]
createUrls Nothing  (Just f) = U [("full", T.unpack f)]
createUrls _ _               = U []
          
-- buildItem r = I (T $ createTitle (parseTitles r, parseArtists r)) (URLS $ parsePreviewUrls r)
-- buildServiceData :: String ->  -> ServiceData
buildServiceData service datas =
                 SD { name  = map toLower $ show service
                    , items = datas
                    }

prepareSpotifyRequest query =
                   (opts, "https://api.spotify.com/v1/search")
                   where
                        opts = defaults & param "q"     .~ [query]
                                        & param "type"  .~ ["track"]
                                        & param "limit" .~ ["5"]
--getServiceData :: (Service, String) -> ServiceData
getServiceData (service, query) =
               let
                  urlComponents = case service of
                                       Spotify -> prepareSpotifyRequest query
               in do
                  r       <- getWith (fst urlComponents) (snd urlComponents)
                  results <- parseData r
                  return $ buildServiceData service results
                  
--search :: String -> [ServiceData]
search query = return $ map getServiceData $ zip dataProviders [query]
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

parseData r = return $ r ^.. responseBody . key "tracks" . key "items" . _Array . traverse . to (\o -> I (createTitle (o ^? key "name" . _String) (o ^.. key "artists" . _Array . traverse . to (\a -> a ^? key "name" . _String))) (createUrls (o ^? key "preview_url" . _String) (o ^? key "external_urls" . key "spotify" . _String)))

-- Request
getQueryParams env =
               let queryArray = map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments (H2.queryString env))
               in [ (a, b) | arr <- queryArray, a <- [head arr], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)

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

app :: H2.Application
app = \env ->
  do
    --countryCode <- findRequestCountryCode $ H2.httpHeaders env
    searchResults  <- search $ getQueryParamValue "q" env
    return $ H2R.set_body_bytestring (encodeUtf8 "") (def { H2.headers = [ ("Content-Type", "text/json") ] })

main = run app
