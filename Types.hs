{-# LANGUAGE DeriveGeneric #-}

module Types (
  OAuth2WebFlow(..),
  OAuth2Tokens(..),
  GoogleUserInfo(..)
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data OAuth2WebFlow = OAuth2WebFlow { scope :: String
                                   , redirectURI :: String
                                   , authURI :: String
                                   , tokenURI :: String
                                   , responseType :: String
                                   , clientId :: String
                                   , clientSecret :: String } deriving (Show)

data OAuth2Tokens = OAuth2Tokens { accessToken :: String
                                 , refreshToken :: Maybe String
                                 , expiresIn :: Integer
                                 , tokenType :: String } deriving (Eq, Show, Generic)

data GoogleUserInfo = GoogleUserInfo { userId :: String
                                     , userEmail :: Maybe String
                                     , userName :: Maybe String } deriving (Eq, Show, Generic)

instance FromJSON OAuth2Tokens
instance FromJSON GoogleUserInfo
instance ToJSON GoogleUserInfo