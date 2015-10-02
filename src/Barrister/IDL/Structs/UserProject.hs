{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.UserProject where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Enums (AccessType)

data UserProject = UserProject
  { project_id :: Int
  , name :: String
  , access_type :: AccessType
  } deriving (Generic)

instance FromJSON UserProject
instance ToJSON UserProject