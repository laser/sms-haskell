{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.CreatePosition where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Structs.CreatePositionProperty (CreatePositionProperty)

data CreatePosition = CreatePosition
  { position_properties :: [CreatePositionProperty]
  } deriving (Generic)

instance FromJSON CreatePosition
instance ToJSON CreatePosition