{-# LANGUAGE DeriveGeneric #-}

module Barrister.IDL.Structs.LoginInfo where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data LoginInfo = LoginInfo
  { access_token :: String
  , expiry_time :: Int
  } deriving (Generic)

instance FromJSON LoginInfo
instance ToJSON LoginInfo