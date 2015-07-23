{-# LANGUAGE OverloadedStrings #-}

module OAuth2 where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson ((.=), (.:), (.:?), object, withObject, eitherDecode)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.String.Conversions (cs)
import Network.HTTP.Conduit (responseBody, setQueryString, urlEncodedBody, parseUrl, method, secure, port, requestHeaders, requestBody, newManager, tlsManagerSettings, httpLbs)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..))
import Network.HTTP.Client (getUri)

import qualified Data.ByteString.Lazy as BL

data OAuth2WebFlow = OAuth2WebFlow { scope :: String
                                   , redirectURI :: String
                                   , authURI :: String
                                   , tokenURI :: String
                                   , responseType :: String
                                   , clientId :: String
                                   , clientSecret :: String } deriving (Show)

type OAuth2Code = BL.ByteString
type OAuth2AccessToken = BL.ByteString

data OAuth2Tokens = OAuth2Tokens { accessToken :: String
                                 , refreshToken :: Maybe String
                                 , expiresIn :: Integer
                                 , tokenType :: String } deriving (Read, Show)

instance FromJSON OAuth2Tokens where
  parseJSON = withObject "oauth2tokens" $ \o -> 
    OAuth2Tokens <$> o .: "access_token"
                 <*> o .:? "refresh_token"
                 <*> o .: "expires_in"
                 <*> o .: "token_type"

getAuthorizationRequest :: OAuth2WebFlow -> IO Request
getAuthorizationRequest flow = do
  request <- parseUrl $ authURI flow

  let params = [ ("scope", Just (cs $ scope flow))
               , ("client_id", Just (cs $ clientId flow))
               , ("redirect_uri", Just (cs $ redirectURI flow))
               , ("response_type", Just (cs $ responseType flow)) ]

  return $ setQueryString params request

getAuthorizationURL :: OAuth2WebFlow -> IO String
getAuthorizationURL flow = do
  request <- getAuthorizationRequest flow
  return . show $ getUri request

getExchangeRequest :: OAuth2WebFlow -> OAuth2Code -> IO Request
getExchangeRequest flow code = do
  request <- parseUrl $ tokenURI flow

  let params = [ ("code", cs code)
               , ("client_id", cs (clientId flow))
               , ("client_secret", cs (clientSecret flow))
               , ("redirect_uri", cs (redirectURI flow))
               , ("grant_type", "authorization_code") ]

  return $ urlEncodedBody params request

issueRequest :: Request -> IO (Response BL.ByteString)
issueRequest request = newManager tlsManagerSettings >>= httpLbs request

getAccessToken :: OAuth2WebFlow -> OAuth2Code -> IO (Either String OAuth2AccessToken)
getAccessToken flow code = do
  response <- getExchangeRequest flow code >>= issueRequest

  let body = responseBody response
      decoded = (eitherDecode body) :: (Either String OAuth2Tokens)

  return $ either (Left) (Right . cs . accessToken) decoded