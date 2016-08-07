module Main where

import qualified Data.Aeson as A    ( decode
                                    , encode )
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe               ( fromJust )
import Happstack.Server         ( askRq
                                , dirs
                                , method
                                , nullConf
                                , ok
                                , simpleHTTP
                                , takeRequestBody
                                , unBody
                                , Method (POST)
                                , ServerPart )

import Control.Monad            (msum)
import Control.Monad.IO.Class   (liftIO)
import JsonModel
import Model
import Lib

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ dirs "parse_json" $ do method POST
                             body <- getBody
                             ok $ parseJSON body
    , dirs "post_data" $ do method POST
                            body <- getBody
                            ok $ body
    , dirs "calculation/rectangular_pot" $ do method POST
                                              body <- getBody
                                              ok $ calculateRectangularPot body
    , ok $ L.pack $ "Hello, World"
    ]

getBody :: ServerPart L.ByteString
getBody = do
    req <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return . L.pack $ ""

parseJSON :: L.ByteString -> L.ByteString
parseJSON body = A.encode $ (fromJust $ A.decode body :: CircularPotCalInput)

calculateRectangularPot :: L.ByteString -> L.ByteString
calculateRectangularPot body = A.encode $ (RectangularClusterPlot (fromJust input) (fromJust output))
    where input = A.decode body :: Maybe RectangularPotCalInput
          output = fmap circlesInRectangleCoord (input >>= verifyRectangularPotCal)
