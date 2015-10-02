{-# LANGUAGE DeriveGeneric #-}

module Types.RPC where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data RPC = Request
  { method  :: String
  , id     :: String
  , version :: String
  } deriving (Generic)

instance FromJSON RPC
instance ToJSON RPC
