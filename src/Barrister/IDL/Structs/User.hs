{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.User where

import           Data.Int                (Int64)
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Enums (Language, GPSFormat, MeasurementSystem, GoogleMapType)

data User = User
  { email :: Maybe String
  , name :: String
  , date_created :: Int64
  , user_id :: String
  , default_language :: Maybe Language
  , default_gps_format :: Maybe GPSFormat
  , default_measurement_system :: Maybe MeasurementSystem
  , default_google_map_type :: Maybe GoogleMapType
  , needs_to_update_settings :: Maybe Bool
  } deriving (Generic)

instance FromJSON User
instance ToJSON User