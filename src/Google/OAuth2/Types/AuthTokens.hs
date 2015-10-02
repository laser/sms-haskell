{-# LANGUAGE DeriveGeneric #-}

module Google.OAuth2.Types.AuthTokens where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

data AuthTokens = AuthTokens
  { access_token  :: String
  , refresh_token :: Maybe String
  , expires_in    :: Integer
  , token_type    :: String
} deriving (Generic)

instance FromJSON AuthTokens
instance ToJSON AuthTokens