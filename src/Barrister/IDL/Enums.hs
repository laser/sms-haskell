{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Enums where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data YesNo = Y | N deriving (Generic)

data FieldType = NUMBER | STRING | IMAGE | IMAGE_LIST deriving (Show, Generic)

data AccessType = OWNER | COLLABORATOR | READONLY | PUBLIC deriving (Show, Generic)

data Language = EN_US | ES_LA | FR_FR deriving (Show, Generic)

data GPSFormat = DECIMAL | DEGREE | UTMWGS84 deriving (Show, Generic)

data MeasurementSystem = METRIC | IMPERIAL deriving (Show, Generic)

data GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN deriving (Show, Generic)

instance FromJSON YesNo
instance ToJSON YesNo

instance FromJSON FieldType
instance ToJSON FieldType

instance FromJSON AccessType
instance ToJSON AccessType

instance FromJSON Language
instance ToJSON Language

instance FromJSON GPSFormat
instance ToJSON GPSFormat

instance FromJSON MeasurementSystem
instance ToJSON MeasurementSystem

instance FromJSON GoogleMapType
instance ToJSON GoogleMapType