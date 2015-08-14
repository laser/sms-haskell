{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Exception (Exception)
import Data.Aeson ((.=), (.:), (.:?), object, withObject, FromJSON(..), ToJSON(..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data OAuth2WebFlow = OAuth2WebFlow { scope :: String
                                   , redirectURI :: String
                                   , authURI :: String
                                   , tokenURI :: String
                                   , responseType :: String
                                   , clientId :: String
                                   , clientSecret :: String } deriving (Eq, Show)

data OAuth2Tokens = OAuth2Tokens { accessToken :: String
                                 , refreshToken :: Maybe String
                                 , expiresIn :: Integer
                                 , tokenType :: String } deriving (Eq, Show)

data GoogleUserInfo = GoogleUserInfo { userId :: String
                                     , userEmail :: Maybe String
                                     , userName :: Maybe String } deriving (Eq, Show)

data GoogleAPIError = RequestError String
                    | ParseError String deriving (Eq, Show)

instance Exception GoogleAPIError

instance FromJSON OAuth2Tokens where
  parseJSON = withObject "oauth2tokens" $ \o ->
    OAuth2Tokens <$> o .: "access_token"
                 <*> o .:? "refresh_token"
                 <*> o .: "expires_in"
                 <*> o .: "token_type"

instance FromJSON GoogleUserInfo where
  parseJSON = withObject "googleuserinfo" $ \o ->
    GoogleUserInfo <$> o .: "id"
                   <*> o .:? "email"
                   <*> o .:? "name"

instance ToJSON GoogleUserInfo where
  toJSON u = object [ "id" .= userId u
                    , "email" .= userEmail u
                    , "name" .= userName u ]