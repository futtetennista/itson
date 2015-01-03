{-# LANGUAGE OverloadedStrings #-}

-- https://github.com/nfjinjing/hack2
import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import Hack2.Handler.SnapServer
import Data.Default (def)
-- http://www.serpentine.com/wreq/
import Network.Wreq hiding (headers)
import Control.Lens
import Data.Aeson.Lens
import Data.Text hiding (foldr, filter, head, tail, reverse, map)
import Data.Text.Encoding

buildJson xs = dropWhileEnd (==',') $ foldr (\e json -> append (append (append (pack "\"") e) (pack "\",")) json) empty xs
buildResponseBody r = append (append (pack "{\"results\":{\"previewUrls\":[") (buildJson $ extractPreviewUrls r)) (pack "]}}")
extractPreviewUrls r = r ^.. responseBody . key "tracks" . key "items" . values . key "preview_url" . _String
getQueryParams (Env _ _ _ queryString _ _ _ _ _ _ _ _) =
                              let queryArray = map (splitOn "=") (splitOn "&" (decodeUtf8 queryString))
                              in [ (a, b) | arr <- queryArray, a <- head arr : [], b <- tail arr]
getQueryParamValue p env = snd . head $ filter (\(a, b) -> a == p) (getQueryParams env)
       
app :: Application
app = \env ->
  let opts = defaults & param "q"     .~ [ getQueryParamValue "q" env ]
                      & param "type"  .~ ["track"]
                      & param "limit" .~ ["5"]
  in
    do r <- getWith opts "https://api.spotify.com/v1/search"
       return $ set_body_bytestring (encodeUtf8 $ buildResponseBody r) (def { headers = [ ("Content-Type", "text/json") ] })

main = run app
