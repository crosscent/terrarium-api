{-# LANGUAGE OverloadedStrings #-}
module Geolocate.JSON
    ( handleGeolocateQuery
    , parseGeolocateQuery
    , GeolocateQuery (..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L    ( pack
                                                    , unpack
                                                    , ByteString )
import qualified Data.ByteString.Char8 as C8        ( unpack )

import Control.Applicative          ( empty )
import Data.Aeson
import Data.Maybe                   ( fromJust )
import Data.Monoid                  ( (<>) )
import Geolocate.Search             ( searchNominatim )

data GeolocateQuery = GeolocateQuery { getQuery :: String
                                     } deriving (Show)

instance FromJSON GeolocateQuery where
    parseJSON (Object v)    = GeolocateQuery <$>
                              v .: "query"
    parseJSON _             = empty

instance ToJSON GeolocateQuery where
    toJSON (GeolocateQuery query) =
        object ["query" .= query]
    toEncoding (GeolocateQuery query) =
        pairs ("query" .= query)

handleGeolocateQuery :: L.ByteString -> (IO L.ByteString)
handleGeolocateQuery body = result $ parseGeolocateQuery body 
    where result Nothing = return ""
          result (Just (GeolocateQuery query)) = fmap L.pack $ searchNominatim query

parseGeolocateQuery :: L.ByteString -> Maybe GeolocateQuery
parseGeolocateQuery body = decode body :: Maybe GeolocateQuery
