{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Data.String.Conversions (cs)
import System.Environment (getEnv)
import Web.Scotty (scotty)

import Routes (routes)
import Types (OAuth2WebFlow(..))

main = do
  prt <- read <$> getEnv "PORT"
  cid <- cs <$> getEnv "SMS_GOOGLE_LOGIN_CLIENT_ID"
  csc <- cs <$> getEnv "SMS_GOOGLE_LOGIN_CLIENT_SECRET"

  let flow = OAuth2WebFlow { scope = "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email"
                           , redirectURI = "http://localhost:5000/oauth2callback"
                           , authURI = "https://accounts.google.com/o/oauth2/auth"
                           , tokenURI = "https://accounts.google.com/o/oauth2/token"
                           , responseType = "code"
                           , clientId = cid
                           , clientSecret = csc }

  scotty prt $ routes flow