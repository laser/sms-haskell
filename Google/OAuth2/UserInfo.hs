{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Google.OAuth2.UserInfo (
  get
) where

import Data.Aeson (eitherDecode)
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (parseUrl, requestHeaders)

import Google.OAuth2.HTTPClient (issueRequest)
import Types (GoogleUserInfo(..), GoogleAPIError(..))

import qualified Data.ByteString as BS

get :: String -> IO (Either GoogleAPIError GoogleUserInfo)
get token = do
  request <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"

  eBody <- issueRequest request { requestHeaders = [("Authorization", BS.append "Bearer " (cs token))] }

  return $ case eBody of
    Left ex -> Left . RequestError $ show ex
    Right body -> case eitherDecode body of
      Left err -> Left $ ParseError err
      Right info -> Right info