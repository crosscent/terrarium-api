{-# LANGUAGE OverloadedStrings #-}
module JsonModel
    ( CircularPotCalInput(..)
    , SquarePotCalInput(..)
    ) where

import Control.Applicative              (empty)
import Data.Monoid                      ((<>))
import Data.Aeson

type InnerRadius = Double
type OuterRadius = Double
type Length = Double
type Width = Double

data CircularPotCalInput = CircularPotCalInput InnerRadius OuterRadius

instance FromJSON CircularPotCalInput where
    parseJSON (Object v) = CircularPotCalInput <$>
                           v .: "inner" <*>
                           v .: "outer"
    parseJSON _          = empty

instance ToJSON CircularPotCalInput where
    toJSON (CircularPotCalInput inner outer) =
        object ["inner" .= inner, "outer" .= outer]
    toEncoding (CircularPotCalInput inner outer) = 
        pairs ("inner" .= inner <> "outer" .= outer)

data SquarePotCalInput = SquarePotCalInput InnerRadius Length Width

instance FromJSON SquarePotCalInput where
    parseJSON (Object v) = SquarePotCalInput <$>
                           v .: "inner" <*>
                           v .: "length" <*>
                           v .: "width"
    parseJSON _          = empty

instance ToJSON SquarePotCalInput where
    toJSON (SquarePotCalInput inner length width) =
        object ["inner" .= inner, "length" .= length, "width" .= width]
    toEncoding (SquarePotCalInput inner length width) = 
        pairs ("inner" .= inner <> "length" .= length <> "width" .= width)
