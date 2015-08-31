{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.Position where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Structs.PositionProperty (PositionProperty)

data Position = Position
  { position_id :: Int
  , project_id :: Int
  , position_properties :: [PositionProperty]
  } deriving (Generic)

instance FromJSON Position
instance ToJSON Position