{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.PositionField where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Enums (FieldType, YesNo)

data PositionField = PositionField
  { position_field_id :: Int
  , field_type :: FieldType
  , name :: String
  , visible :: YesNo
  } deriving (Generic)

instance FromJSON PositionField
instance ToJSON PositionField