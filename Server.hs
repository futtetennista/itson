{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import qualified Hack2 as H2 (Application(..), queryString, headers, httpHeaders)
import qualified Hack2.Contrib.Response as H2R (set_body_bytestring)
import qualified Hack2.Handler.SnapServer as Server (run)
import Data.Default (def)
-- http://www.serpentine.com/wreq/
import qualified Network.Wreq as Wreq (Response(..), responseBody, defaults, param, get, getWith)
import Network.HTTP.Types.URI (decodePathSegments)
import Control.Lens
import Data.Aeson.Lens
import qualified Data.Aeson.Types as AT (Value(String), emptyArray)
import qualified Data.Text as T (Text(..), dropWhileEnd, append, empty, splitOn, unpack, pack, intercalate)
import qualified Data.ByteString.Char8 as C8 (pack, unpack, split)
import qualified Data.ByteString as BS (ByteString(..), empty, unpack)
--import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Aeson as A
import Data.Char (toLower)

-- Models
data DataProvider = Spotify deriving Show
dataProviders = [Spotify]

data Urls  = U [(T.Text, T.Text)] deriving Show
data Title = TT T.Text deriving Show
data Item = I Title Urls | Empty deriving Show
data Fact = F { name  :: T.Text
              , items :: [Item]
              } deriving Show
data Result = Res [Fact] deriving Show

instance A.ToJSON Item where
         toJSON (I (TT title) (U kvs)) = A.object $ ("title" A..= title) : ["urls" A..= A.object (map (\(k, v) -> k A..= v) kvs)]
instance A.ToJSON Fact where
         toJSON (F name [])    = A.object ["service" A..= name, "items" A..= AT.emptyArray]
         toJSON (F name items) = A.object ["service" A..= name, "items" A..= items]
instance A.ToJSON Result where
         toJSON (Res body) = A.object ["results" A..= body]

-- Serialisation
intercalateM sep xs = T.intercalate sep . filter (/="") $ map (\ m -> case m of { Just v -> v; Nothing -> "" }) xs
createTitle :: Maybe T.Text -> [Maybe T.Text] -> Title
createTitle (Just t) (a:as) = TT $ t `T.append` " by " `T.append` (intercalateM ", " (a:as))
createTitle (Just t) []     = TT t
createTitle Nothing  _      = TT "Song Details Unknown"

createUrls :: Bool -> Maybe T.Text -> Maybe T.Text -> Urls
createUrls True (Just p) (Just f) = U [("preview", p), ("full", f)]
createUrls True Nothing  (Just f) = U [("full", f)]
createUrls False (Just p) _       = U [("preview", p)]
createUrls False _ _              = U []

-- Parsing
parseAvailableMarkets :: (AsValue v) => v -> [T.Text]
parseAvailableMarkets v = v ^. key "available_markets" . _Array . traverse . to (\ms -> ms ^.. _String)
isPlayable :: (AsValue v) => T.Text -> v -> Bool
isPlayable cc vs = not . null . filter (==cc) $ parseAvailableMarkets vs
parseDataProviderResponse :: (AsValue body) => DataProvider -> T.Text -> Wreq.Response body -> [Item]
parseDataProviderResponse service cc resp =
          resp ^.. Wreq.responseBody . key "tracks" . key "items" . _Array . traverse . to (\o -> I (createTitle (o ^? key "name" . _String) (o ^.. key "artists" . _Array . traverse . to (\a -> a ^? key "name" . _String))) (createUrls (isPlayable cc o) (o ^? key "preview_url" . _String) (o ^? key "external_urls" . key "spotify" . _String)))

-- Incoming Request
getQueryParams env =
               let queryArray = map (T.splitOn "=") $ T.splitOn "&" (head $ decodePathSegments (H2.queryString env))
               in [ (a, b) | arr <- queryArray, a <- [head arr], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)

--printableRequestHeaders headers = foldr (\(k, v) hx -> mappend (mappend hx $ mappend (byteString k) (mappend (byteString "=") (byteString v))) (byteString "\n")) (byteString BS.empty) headers
--requestHeadersToBS headers = BSL.toStrict $ toLazyByteString (printableRequestHeaders headers)
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
                         r <- Wreq.get $ "http://www.telize.com/geoip/2.240.222.127"
                         -- r <- get $ "http://www.telize.com/geoip/" ++ getIpFromRequest headers
                         return $ r ^. Wreq.responseBody . key "country_code" . _String

createFact :: DataProvider -> [Item] -> Fact
createFact dataProvider xs = F { name  = T.pack . map toLower $ show dataProvider
                                , items = xs
                                }

--search :: T.Text -> [ServiceData]
--search query = map getServiceData $ zip dataProviders [query]

--resultsToJSON :: Response -> Data.ByteString.Lazy
resultsToJSON b = A.encode b
bodyByteString :: Result -> BS.ByteString
bodyByteString res = BSL.toStrict $ resultsToJSON res

createSpotifyRequest searchTerm =
                     (opts, "https://api.spotify.com/v1/search")
                     where
                          opts = Wreq.defaults & Wreq.param "q"     .~ [searchTerm]
                                               & Wreq.param "type"  .~ ["track"]
                                               & Wreq.param "limit" .~ ["5"]

app :: H2.Application
app = \env ->
    let q = getQueryParamValue "term" env
        urlComponents = createSpotifyRequest q
    in do
        --putStrLn $ "Searching results for term: \"" ++ T.unpack q ++ "\""
        cc <- findRequestCountryCode $ H2.httpHeaders env
        r <- Wreq.getWith (fst urlComponents) (snd urlComponents)
        --putStrLn "got response from spotify. Parsing..."
        let results = parseDataProviderResponse Spotify cc r
        --putStrLn "parsing done...creating response"
        let searchResults = Res [createFact Spotify results]
        let body = bodyByteString searchResults
        --putStrLn $ C8.unpack body
        --let searchResults = search $ getQueryParamValue "q" env
        return $ H2R.set_body_bytestring body (def { H2.headers = [ ("Content-Type", "text/json") ] })

main = Server.run app

{-|
Spotify Tests:
* /?term=Damballah -> the last result is not available in the test country
* /?term=glück -> utf-8 encoding
* /?term=glück123 -> empty results
-}
