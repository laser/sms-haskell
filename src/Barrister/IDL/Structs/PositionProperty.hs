{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.PositionProperty where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Enums (FieldType, YesNo)

data PositionProperty = PositionProperty
  { property_id :: Int
  , field_type :: FieldType
  , name :: String
  , value :: string
  , visible :: YesNo
} deriving (Generic)

instance FromJSON PositionProperty
instance ToJSON PositionProperty