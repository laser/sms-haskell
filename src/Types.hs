{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Applicative     (pure)
import           Control.Exception       (Exception)
import           Data.Aeson              ((.:), (.:?), (.=))
import qualified Data.Aeson              as A
import           Data.String.Conversions (cs)
import           Data.Typeable           (Typeable)

import qualified Data.ByteString         as BS
import qualified Data.Text.Lazy          as TL

data OAuth2WebFlow = OAuth2WebFlow { scope        :: String
                                   , authURI      :: String
                                   , tokenURI     :: String
                                   , responseType :: String
                                   , redirectURI  :: String
                                   , clientId     :: String
                                   , clientSecret :: String } deriving (Eq, Show)

data OAuth2Tokens = OAuth2Tokens { accessToken  :: String
                                 , refreshToken :: Maybe String
                                 , expiresIn    :: Integer
                                 , tokenType    :: String } deriving (Eq, Show)

data GoogleUserInfo = GoogleUserInfo { userId    :: String
                                     , userEmail :: Maybe String
                                     , userName  :: Maybe String } deriving (Eq, Show)

data JSONDecodeError = JSONDecodeError String deriving (Eq, Show, Typeable)

data WebAppException = WAEError String deriving (Eq, Show, Typeable)

instance A.FromJSON OAuth2Tokens where
  parseJSON (A.Object o) = OAuth2Tokens <$> o .: "access_token"
                                      <*> o .:? "refresh_token"
                                      <*> o .: "expires_in"
                                      <*> o .: "token_type"

instance A.FromJSON GoogleUserInfo where
  parseJSON (A.Object o) = GoogleUserInfo <$> o .: "id"
                                        <*> o .:? "email"
                                        <*> o .:? "name"

instance A.ToJSON GoogleUserInfo where
  toJSON u = A.object [ "id" .= userId u
                    , "email" .= userEmail u
                    , "name" .= userName u ]

instance Exception JSONDecodeError
instance Exception WebAppException

data RPC = Request { method  :: String
                   , cid     :: String
                   , version :: String } deriving (Eq, Show)

instance A.FromJSON RPC where
  parseJSON (A.Object o) = Request <$> o .: "method"
                                   <*> o .: "id"
                                   <*> o .: "jsonrpc"

data Project = Project { project_project_id :: Integer
                       , project_name       :: String } deriving (Show, Eq)

data UserProject = UserProject { userproject_project_id  :: Integer
                               , userproject_name        :: String
                               , userproject_access_type :: AccessType } deriving (Show, Eq)


instance A.ToJSON Project where
  toJSON p = A.object [ "project_id" .= project_project_id p
                      , "name" .= project_name p]

instance A.ToJSON UserProject where
  toJSON p = A.object [ "project_id" .= userproject_project_id p
                      , "name" .= userproject_name p
                      , "access_type" .= userproject_access_type p]

instance A.FromJSON Project where
  parseJSON (A.Object o) = Project <$> o .: "project_id" <*> o .: "name"

instance A.FromJSON UserProject where
  parseJSON (A.Object o) = UserProject <$> o .: "project_id" <*> o .: "name" <*> o .: "access_type"

data User = User { user_user_id                    :: String
                 , user_email                      :: Maybe String
                 , user_name                       :: String
                 , user_date_created               :: Integer
                 , user_default_language           :: Maybe Language
                 , user_default_gps_format         :: Maybe GPSFormat
                 , user_default_measurement_system :: Maybe MeasurementSystem
                 , user_default_google_map_type    :: Maybe GoogleMapType
                 , user_needs_to_update_settings   :: Maybe Bool } deriving (Show, Eq)

data AccessType = OWNER | COLLABORATOR | READONLY | PUBLIC deriving (Eq, Show)

data Language = EN_US | ES_LA | FR_FR deriving (Eq, Show)

data GPSFormat = DECIMAL | DEGREE | UTMWGS84 deriving (Eq, Show)

data MeasurementSystem = METRIC | IMPERIAL deriving (Eq, Show)

data GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN deriving (Eq, Show)

instance A.FromJSON AccessType where
  parseJSON (A.String t) = pure $ fromString $ TL.unpack $ TL.fromStrict t
    where fromString "OWNER" = OWNER
          fromString "COLLABORATOR" = COLLABORATOR
          fromString "READONLY" = READONLY
          fromString "PUBLIC" = PUBLIC

instance A.ToJSON GoogleMapType where
  toJSON = A.String . cs . show

instance A.ToJSON Language where
  toJSON = A.String . cs . show

instance A.ToJSON GPSFormat where
  toJSON = A.String . cs . show

instance A.ToJSON MeasurementSystem where
  toJSON = A.String . cs . show

instance A.ToJSON AccessType where
  toJSON = A.String . cs . show

instance A.ToJSON User where
  toJSON u = A.object [ "user_id" .= user_user_id u
                      , "email" .= user_email u
                      , "name" .= user_name u
                      , "date_created" .= user_date_created u
                      , "default_language" .= user_default_language u
                      , "default_gps_format" .= user_default_gps_format u
                      , "default_measurement_system" .= user_default_measurement_system u
                      , "default_google_map_type" .= user_default_google_map_type u
                      , "needs_to_update_settings" .= user_needs_to_update_settings u ]
