module Geolocate.Search 
    ( searchNominatim
    )where

import qualified Network.HTTP as HTTP       ( getResponseBody
                                            , simpleHTTP )
                                       
import qualified HTTP.Base as CustomHTTP    ( getRequest )


searchNominatim :: String -> IO (String)
searchNominatim query = HTTP.simpleHTTP (CustomHTTP.getRequest $ url) >>= HTTP.getResponseBody
    where url = "http://nominatim.openstreetmap.org/search?format=json&addressdetails=1&q=" ++ query
