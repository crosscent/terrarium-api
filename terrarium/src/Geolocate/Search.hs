{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Geolocate.Search 
    ( searchNominatim
    )where

import qualified Data.ByteString.Lazy.Char8 as L    ( pack
                                                    , ByteString )
import qualified Network.HTTP as HTTP               ( getResponseBody
                                                    , simpleHTTP )
import qualified Network.HTTP.Base as HTTP.Base     ( urlEncode )
                                       
import qualified HTTP.Base as CustomHTTP            ( getRequest )
import Control.Applicative                          ( empty )
import Data.Aeson
import qualified Data.Aeson.Types as Aeson.Types    ( Parser (..) )
import Data.Maybe                                   ( fromMaybe )
import Data.Monoid                                  ( (<>) )
import Data.Int                                     ( Int64 )
import Data.Sequence                                ( fromList )
import qualified Data.Text as Text                  ( unpack )

type OSM_id = Integer

data Coordinates = Coordinates Double Double
data Polygon = Polygon [Coordinates]

data NominatimResult = NominatimResult
    { osm_id :: Integer
    , place_id :: Integer
    , osm_type :: String
    , importance :: Double
    , display_name :: String
    , polygon_points :: Polygon
    }

instance FromJSON Coordinates where
    parseJSON v = do
        [x, y] <- parseJSON v
        case (x, y) of
          ((Number x), (Number y)) -> return $ Coordinates (read $ show x) (read $ show y)
          ((String x), (String y)) -> return $ Coordinates (read $ Text.unpack $ x) (read $ Text.unpack $ y)

instance FromJSON Polygon where
    parseJSON json = do
        x <- parseJSON json
        return $ Polygon x

instance ToJSON Coordinates where
    toJSON (Coordinates x y) = 
        object ["x" .= x, "y" .= y]


instance FromJSON NominatimResult where
    parseJSON (Object v) = NominatimResult <$>
                           fmap (read) (v .: "osm_id") <*>
                           fmap (read) (v .: "place_id") <*>
                           v .: "osm_type" <*>
                           v .: "importance" <*>
                           v .: "display_name" <*>
                           v .:? "polygonpoints" .!= Polygon []
    parseJSON _          = empty

instance ToJSON NominatimResult where
    toJSON (NominatimResult osm_id place_id osm_type importance display_name polygon_points) =
        object [ "osm_id" .= osm_id
               , "place_id" .= place_id
               , "osm_type" .= osm_type
               , "importance" .= importance
               , "display_name" .= display_name
               , "polygon_points" .= toList polygon_points]
       where toList (Polygon xs) = fromList xs

filterNominatim :: [NominatimResult] -> Maybe NominatimResult
filterNominatim [] = Nothing
filterNominatim (x:xs)
  | osm_type x  == "relation"   = Just x
  | otherwise                   = filterNominatim xs

escapeSpace :: String -> String
escapeSpace [] = []
escapeSpace query = takeWhile (/= ' ') query ++  escapeSpace remainder
    where remainder = case dropWhile (/= ' ') query of
            [] -> []
            x -> "%20" ++ tail x

searchNominatim :: String -> IO (L.ByteString)
searchNominatim query = do
    result <- HTTP.simpleHTTP (CustomHTTP.getRequest $ url) >>= HTTP.getResponseBody
    case decode(L.pack result) :: Maybe [NominatimResult] of
        Nothing -> return $ L.pack $ "An error has occured: JSON returned from Nominatim could not be parsed: " ++ result
        Just results -> do
            case filterNominatim results of
              Nothing -> return $ encode $ head results
              Just x -> return $ encode x
    where url = "http://nominatim.openstreetmap.org/search?format=json&polygon=1&addressdetails=1&q=" ++ (HTTP.Base.urlEncode query)

getPolygon :: OSM_id -> IO (String)
getPolygon id = HTTP.simpleHTTP(CustomHTTP.getRequest $ url) >>= HTTP.getResponseBody
    where url = ""
