{-# LANGUAGE OverloadedStrings #-}
module JsonModel
    ( CircularPotCal
    ) where

import Control.Applicative              (empty)
import Data.Monoid                      ((<>))
import Data.Aeson

data CircularPotCal = CircularPotCal { inner :: Int
                                      , outer :: Int}

instance FromJSON CircularPotCal where
    parseJSON (Object v) = CircularPotCal <$>
                           v .: "inner" <*>
                           v .: "outer"
    parseJSON _          = empty

instance ToJSON CircularPotCal where
    toJSON (CircularPotCal inner outer) =
        object ["inner" .= inner, "outer" .= outer]
    toEncoding (CircularPotCal inner outer) = 
        pairs ("inner" .= inner <> "outer" .= outer)
