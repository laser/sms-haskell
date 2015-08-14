{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module OAuth2 (
  getAuthorizationURL,
  getAccessToken,
  getUserInfo
) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import Network.HTTP.Conduit (responseBody, setQueryString, urlEncodedBody, parseUrl, method, secure, port, requestHeaders, requestBody, newManager, tlsManagerSettings, httpLbs)
import Network.HTTP.Conduit (Request(..), RequestBody(..), Response(..), HttpException(..))
import Network.HTTP.Client (getUri)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Types (GoogleUserInfo(..), OAuth2WebFlow(..), OAuth2Tokens(..))

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

getExchangeRequest :: OAuth2WebFlow -> String -> IO Request
getExchangeRequest flow code = do
  request <- parseUrl $ tokenURI flow

  let params = [ ("code", cs code)
               , ("client_id", cs $ clientId flow)
               , ("client_secret", cs $ clientSecret flow)
               , ("redirect_uri", cs $ redirectURI flow)
               , ("grant_type", "authorization_code") ]

  return $ urlEncodedBody params request

getAccessToken :: OAuth2WebFlow -> String -> IO (Either String String)
getAccessToken flow code = do

  eBody <- getExchangeRequest flow code >>= issueRequest

  return $ either (Left . show) (either (Left) (Right . cs . accessToken) . eitherDecode) eBody

getUserInfo :: String -> IO (Either String GoogleUserInfo)
getUserInfo token = do
  request <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"

  eBody <- issueRequest request { requestHeaders = [("Authorization", BS.append "Bearer " (cs token))] }

  return $ either (Left . show) (eitherDecode) eBody