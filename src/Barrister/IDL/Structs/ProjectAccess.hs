{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.ProjectAccess where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Barrister.IDL.Enums (AccessType)

data ProjectAccess = ProjectAccess
  { project_access_id :: Int
  , project_id :: Int
  , access_type :: AccessType
  , user_id :: Maybe String
  , email :: Maybe String
  , link :: Maybe String
  } deriving (Generic)

instance FromJSON ProjectAccess
instance ToJSON ProjectAccess