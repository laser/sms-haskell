{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.Project where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data Project = Project
  { project_id :: Int
  , name :: String
  } deriving (Generic)

instance FromJSON Project
instance ToJSON Project