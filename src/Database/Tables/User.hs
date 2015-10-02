module Database.Tables.User where

import           Data.Int                (Int64)

import Barrister.IDL.Enums (Language, GPSFormat, MeasurementSystem, GoogleMapType)

data User = User
  { user_id :: String
  , email :: Maybe String
  , name :: Maybe String
  , date_created :: Int64
  , default_language :: Maybe Language
  , default_gps_format :: Maybe GPSFormat
  , default_measurement_system :: Maybe MeasurementSystem
  , default_google_map_type :: GoogleMapType
  }
