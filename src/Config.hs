module Config (
  getConfig,
  Config(..),
  ServerConfig(..),
  BarristerConfig(..)
) where

import           Control.Applicative
import           System.Environment

import Google.OAuth2.Types.AuthFlow( AuthFlow(..) )

data Config = Config { serverConfig    :: ServerConfig
                     , oauthConfig     :: AuthFlow
                     , barristerConfig :: BarristerConfig } deriving (Show)

data ServerConfig = ServerConfig { serverHost :: String
                                 , serverPort :: Int } deriving (Show)

data BarristerConfig = BarristerConfig { idl :: String } deriving (Show)

getConfig :: IO Config
getConfig = Config <$> getServerConfig <*> getOAuth2Config <*> getBarristerConfig
  where
    getServerConfig =
      ServerConfig <$> getEnv "SMS_HOST"
                   <*> (read <$> getEnv "SMS_PORT")

    getOAuth2Config =
      WebFlow "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email"
                    "https://accounts.google.com/o/oauth2/auth"
                    "https://accounts.google.com/o/oauth2/token"
                    "code"
                    <$> getRedirectURI
                    <*> getEnv "SMS_GOOGLE_LOGIN_CLIENT_ID"
                    <*> getEnv "SMS_GOOGLE_LOGIN_CLIENT_SECRET"

    getRedirectURI =
      (\host port -> "http://" ++ host ++ ":" ++ port ++ "/oauth2callback") <$> getEnv "SMS_HOST"
                                                                            <*> getEnv "SMS_PORT"

    getBarristerConfig = BarristerConfig <$> readFile "./sms.json"
