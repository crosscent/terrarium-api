module Main where

import Data.ByteString.Lazy.Char8 (unpack)
import Happstack.Server         ( askRq
                                , method
                                , nullConf
                                , ok
                                , simpleHTTP
                                , takeRequestBody
                                , unBody
                                , Method (POST)
                                , ServerPart)

import Control.Monad            (msum)
import Control.Monad.IO.Class   (liftIO)
import Lib

main :: IO ()
main = simpleHTTP nullConf $ msum
    [ do method POST
         body <- getBody
         ok $ body
    , ok $ "Hello, World"
    ]

getBody :: ServerPart [Char]
getBody = do
    req <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqbody -> return . unpack $ unBody $ rqbody
        Nothing     -> return ""
