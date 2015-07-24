{-# LANGUAGE OverloadedStrings #-}

module OAuth2 where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.=), (.:), (.:?), object, withObject, eitherDecode)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.String.Conversions (cs)
import Network.HTTP.Conduit (responseBody, setQueryString, urlEncodedBody, parseUrl, method, secure, port, requestHeaders, requestBody, newManager, tlsManagerSettings, httpLbs)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..), HttpException(..))
import Network.HTTP.Client (getUri)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

data OAuth2WebFlow = OAuth2WebFlow { scope :: String
                                   , redirectURI :: String
                                   , authURI :: String
                                   , tokenURI :: String
                                   , responseType :: String
                                   , clientId :: String
                                   , clientSecret :: String } deriving (Show)

type OAuth2Code = String
type OAuth2AccessToken = String

data OAuth2Tokens = OAuth2Tokens { accessToken :: String
                                 , refreshToken :: Maybe String
                                 , expiresIn :: Integer
                                 , tokenType :: String } deriving (Show)

instance FromJSON OAuth2Tokens where
  parseJSON = withObject "oauth2tokens" $ \o -> 
    OAuth2Tokens <$> o .: "access_token"
                 <*> o .:? "refresh_token"
                 <*> o .: "expires_in"
                 <*> o .: "token_type"

data GoogleUserInfo = GoogleUserInfo { userId :: String
                                     , userEmail :: Maybe String
                                     , userName :: Maybe String } deriving (Show)

instance FromJSON GoogleUserInfo where
  parseJSON = withObject "googleuserinfo" $ \o ->
    GoogleUserInfo <$> o .: "id"
                   <*> o .:? "email"
                   <*> o .:? "name"

instance ToJSON GoogleUserInfo where
  toJSON u = object [ "id" .= userId u
                    , "email" .= userEmail u
                    , "name" .= userName u ]

issueRequest :: Request -> IO (Either HttpException BL.ByteString)
issueRequest request = liftIO $ catch (issue request) (return . Left)
  where 
    issue r = do
      response <- newManager tlsManagerSettings >>= httpLbs r
      return . Right $ responseBody response

getAuthorizationRequest :: OAuth2WebFlow -> IO Request
getAuthorizationRequest flow = do
  request <- parseUrl $ authURI flow

  let params = [ ("scope", Just . cs $ scope flow)
               , ("client_id", Just . cs $ clientId flow)
               , ("redirect_uri", Just . cs $ redirectURI flow)
               , ("response_type", Just . cs $ responseType flow) ]

  return $ setQueryString params request

getAuthorizationURL :: OAuth2WebFlow -> IO String
getAuthorizationURL flow = getAuthorizationRequest flow >>= return . show . getUri

getExchangeRequest :: OAuth2WebFlow -> OAuth2Code -> IO Request
getExchangeRequest flow code = do
  request <- parseUrl $ tokenURI flow

  let params = [ ("code", cs code)
               , ("client_id", cs $ clientId flow)
               , ("client_secret", cs $ clientSecret flow)
               , ("redirect_uri", cs $ redirectURI flow)
               , ("grant_type", "authorization_code") ]

  return $ urlEncodedBody params request

getAccessToken :: OAuth2WebFlow -> OAuth2Code -> IO (Either String OAuth2AccessToken)
getAccessToken flow code = do

  eBody <- getExchangeRequest flow code >>= issueRequest

  return $ either (Left . show) (either (Left) (Right . cs . accessToken) . eitherDecode) eBody

getUserInfo :: OAuth2AccessToken -> IO (Either String GoogleUserInfo)
getUserInfo token = do
  request <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"

  eBody <- issueRequest request { requestHeaders = [("Authorization", BS.append "Bearer " (cs token))] }

  return $ either (Left . show) (eitherDecode) eBody