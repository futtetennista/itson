{-# LANGUAGE OverloadedStrings #-}
module Utils.Properties (getProperty) where

import System.IO (IOMode(ReadMode), withFile)
import Data.Text.IO (hGetContents)
import qualified Data.Text as T (pack, unpack, splitOn, lines)
import Data.Text (Text)


getProperty :: String -> IO (Either String String)
getProperty key =
  do withFile "config.properties" ReadMode
       (\handle ->
         do contents <- hGetContents handle
            case getKey (T.pack key) contents of
              [entry] ->
                return $ Right (T.unpack entry)

              (_:_) ->
                return (Left $ "Multiple values for '" ++ key ++ "'")

              _     ->
                return (Left $"'" ++ key ++ "' key not found. \
\ Possible issues are: \n                                     \
\ 1) config.properties file is missing \n                     \
\ 2) the configuration file doesn't have a " ++ key ++ " entry \n"))


getKey :: Text -> Text -> [Text]
getKey key properties =
  value . filterConsumerKey . toPair . toArray $ properties

  where value kvs =
          case kvs of
            (Just (_, v)):xs -> [v] ++ value xs
            _                -> []

        filterConsumerKey xs =
          filter (\x ->
                   case x of
                     Nothing     -> False
                     Just (k, _) ->
                       if (k == key)
                       then True
                       else False) xs
        toPair arr =
          map (\xs ->
                case xs of
                  [k, v] -> Just (k, v)
                  _      -> Nothing) arr

        toArray properties =
          map (T.splitOn "=") (T.lines properties)
