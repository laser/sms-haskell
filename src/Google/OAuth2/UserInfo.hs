{-# LANGUAGE OverloadedStrings #-}

module Google.OAuth2.UserInfo (
  get
) where

import           Control.Arrow              (left)
import           Control.Exception          (SomeException, toException)
import           Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (append)

import           Control.Error.Util         (hoistEither)
import           Data.Aeson                 (eitherDecode)
import           Data.String.Conversions    (cs)
import           Network.HTTP.Conduit       (parseUrl, requestHeaders)

import           Google.OAuth2.APIClient
import qualified Google.OAuth2.Types.GoogleUserInfo as G
import qualified Types.JSONDecodeError as J

get :: String -> ExceptT SomeException IO G.GoogleUserInfo
get token = do
  req  <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"
  body <- issueRequest req { requestHeaders = [("Authorization", append "Bearer " (cs token))] }

  hoistEither $ left (toException . J.JSONDecodeError) $ eitherDecode body
