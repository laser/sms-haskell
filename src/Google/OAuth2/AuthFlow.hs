{-# LANGUAGE OverloadedStrings #-}

module Google.OAuth2.AuthFlow (
  getAuthorizationURL,
  getAccessToken
) where

import           Control.Arrow              (left)
import           Control.Exception          (SomeException (..), toException)
import           Control.Monad              (liftM)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT)

import           Control.Error              (syncIO)
import           Control.Error.Util         (hoistEither)
import           Data.Aeson                 (eitherDecode)
import           Data.String.Conversions    (cs)
import           Network.HTTP.Client        (getUri)
import           Network.HTTP.Conduit       (HttpException (..), Request (..),
                                             RequestBody (..), Response (..),
                                             parseUrl, setQueryString,
                                             urlEncodedBody)

import           Google.OAuth2.APIClient
import qualified Google.OAuth2.Types.AuthFlow as A
import qualified Google.OAuth2.Types.AuthTokens as T
import qualified Types.JSONDecodeError as J

getAuthorizationRequest :: A.AuthFlow -> ExceptT SomeException IO Request
getAuthorizationRequest flow@(A.WebFlow s a t rt ru ci cs') = syncIO $ do
  request <- parseUrl $ a

  let params = [ ("scope", Just $ cs s)
               , ("client_id", Just $ cs ci)
               , ("redirect_uri", Just $ cs ru)
               , ("response_type", Just $ cs rt) ]

  return $ setQueryString params request

getExchangeRequest :: A.AuthFlow -> String -> ExceptT SomeException IO Request
getExchangeRequest flow@(A.WebFlow s a t rt ru ci cs') code = syncIO $ do
  request <- parseUrl $ t

  let params = [ ("code", cs code)
               , ("client_id", cs ci)
               , ("client_secret", cs cs')
               , ("redirect_uri", cs ru)
               , ("grant_type", "authorization_code") ]

  return $ urlEncodedBody params request

getAuthorizationURL :: A.AuthFlow -> ExceptT SomeException IO String
getAuthorizationURL flow = do
  req <- getAuthorizationRequest flow
  return . show . getUri $ req

getAccessToken :: A.AuthFlow -> String -> ExceptT SomeException IO String
getAccessToken flow code = do
  request <- getExchangeRequest flow code
  body    <- issueRequest request
  info    <- hoistEither $ left (toException . J.JSONDecodeError) $ eitherDecode body
  return . cs . T.access_token $ info
