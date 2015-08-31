{-# LANGUAGE OverloadedStrings #-}

module Google.OAuth2.UserInfo (
  get
) where

import           Control.Arrow              (left)
import           Control.Exception          (SomeException, toException)
import           Control.Monad.Trans.Except (ExceptT)

import           Control.Error.Util         (hoistEither)
import           Data.Aeson                 (eitherDecode)
import           Data.String.Conversions    (cs)
import           Network.HTTP.Conduit       (parseUrl, requestHeaders)

import           Google.OAuth2.APIClient
import           Types

import qualified Data.ByteString            as BS

get :: String -> ExceptT SomeException IO GoogleUserInfo
get token = do
  req  <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"
  body <- issueRequest req { requestHeaders = [("Authorization", BS.append "Bearer " (cs token))] }

  hoistEither $ left (toException . JSONDecodeError) $ eitherDecode body
