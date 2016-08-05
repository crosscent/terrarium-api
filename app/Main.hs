module Main where

import Happstack.Server ( nullConf
                        , simpleHTTP
                        , toResponse
                        , ok)
import Lib

main :: IO ()
main = simpleHTTP nullConf $ ok "Hello, World"
