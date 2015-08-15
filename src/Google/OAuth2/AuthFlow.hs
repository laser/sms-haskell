{-# LANGUAGE OverloadedStrings #-}

module Google.OAuth2.AuthFlow (
  getAuthorizationURL,
  getAccessToken
) where

import Control.Arrow (left)
import Control.Exception (toException, SomeException(..))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Error.Util (hoistEither)
import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import Network.HTTP.Client (getUri)
import Network.HTTP.Conduit (urlEncodedBody, parseUrl, setQueryString, Request(..), RequestBody(..), Response(..), HttpException(..))

import Google.OAuth2.APIClient (issueRequest)
import Types (OAuth2WebFlow(..), OAuth2Tokens(..), JSONDecodeError(..))

getAuthorizationRequest :: OAuth2WebFlow -> IO Request
getAuthorizationRequest flow = do
  request <- parseUrl $ authURI flow

  let params = [ ("scope", Just . cs $ scope flow)
               , ("client_id", Just . cs $ clientId flow)
               , ("redirect_uri", Just . cs $ redirectURI flow)
               , ("response_type", Just . cs $ responseType flow) ]

  return $ setQueryString params request

getExchangeRequest :: OAuth2WebFlow -> String -> IO Request
getExchangeRequest flow code = do
  request <- parseUrl $ tokenURI flow

  let params = [ ("code", cs code)
               , ("client_id", cs $ clientId flow)
               , ("client_secret", cs $ clientSecret flow)
               , ("redirect_uri", cs $ redirectURI flow)
               , ("grant_type", "authorization_code") ]

  return $ urlEncodedBody params request

getAuthorizationURL :: OAuth2WebFlow -> IO String
getAuthorizationURL flow = getAuthorizationRequest flow >>= return . show . getUri

getAccessToken :: OAuth2WebFlow -> String -> ExceptT SomeException IO String
getAccessToken flow code = do
  request <- liftIO $ getExchangeRequest flow code
  body    <- issueRequest request
  info    <- hoistEither $ left (toException . JSONDecodeError) $ eitherDecode body
  return . cs . accessToken $ info