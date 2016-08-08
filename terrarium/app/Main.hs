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

import Happstack.Server.Internal.Types  ( logMAccess
                                        , Conf(..))

import Control.Monad            (msum)
import Control.Monad.IO.Class   (liftIO)
import System.Environment       (getArgs)
import JsonModel
import Model
import Lib

type Port = Int

main :: IO ()
main = do
    args <- getArgs
    let conf = genConf (read $ head args :: Int)
    server conf

genConf :: Port -> Conf
genConf port = Conf port Nothing (Just logMAccess) 30 Nothing

server :: Conf -> IO ()
server conf = simpleHTTP conf $ msum
    [ dirs "parse_json" $ do method POST
                             body <- getBody
                             ok $ parseJSON body
    , dirs "post_data" $ do method POST
                            body <- getBody
                            ok $ body
    , dirs "calculation/rectangular_pot" $ do method POST
                                              body <- getBody
                                              ok $ calculateRectangularPot body
    , dirs "calculation/circular_pot" $ do method POST
                                           body <- getBody
                                           ok $ calculateCircularPot body
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

calculateCircularPot :: L.ByteString -> L.ByteString
calculateCircularPot body = A.encode $ (CircularClusterPlot (fromJust input) (fromJust output))
    where input = A.decode body :: Maybe CircularPotCalInput
          output = fmap circlesInCircleCoord (input >>= verifyCircularPotCal)

calculateRectangularPot :: L.ByteString -> L.ByteString
calculateRectangularPot body = A.encode $ (RectangularClusterPlot (fromJust input) (fromJust output))
    where input = A.decode body :: Maybe RectangularPotCalInput
          output = fmap circlesInRectangleCoord (input >>= verifyRectangularPotCal)
