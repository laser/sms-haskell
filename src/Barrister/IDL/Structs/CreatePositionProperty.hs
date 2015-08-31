{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.CreatePositionProperty where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data CreatePositionProperty = CreatePositionProperty
  { name :: String
  , value :: String
  } deriving (Generic)

instance FromJSON CreatePositionProperty
instance ToJSON CreatePositionProperty