{-# LANGUAGE DeriveGeneric #-}

module Google.OAuth2.Types.GoogleUserInfo where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data GoogleUserInfo = GoogleUserInfo
  { id :: String
  , email :: Maybe String
  , name :: Maybe String
} deriving (Generic)

instance FromJSON GoogleUserInfo
instance ToJSON GoogleUserInfo