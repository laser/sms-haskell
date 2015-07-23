{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans (liftIO)
import Data.String.Conversions (cs)
import System.Environment (getEnvironment)

import qualified Web.Scotty as Scotty

import OAuth2 (OAuth2WebFlow(..), getAuthorizationURL, getExchangeRequest, getAccessToken)
import RPCService (handleRPC)

main = do
  env <- getEnvironment

  let port = maybe 3000 read $ lookup "PORT" env
      x = maybe "" id $ lookup "GOOGLE_OAUTH_CLIENT_ID" env
      y = maybe "" id $ lookup "GOOGLE_OAUTH_CLIENT_SECRET" env
      flow = OAuth2WebFlow { scope = "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email"
                           , redirectURI = "http://localhost:5000/oauth2callback"
                           , authURI = "https://accounts.google.com/o/oauth2/auth"
                           , tokenURI = "https://accounts.google.com/o/oauth2/token"
                           , responseType = "code"
                           , clientId = cs x
                           , clientSecret = cs y }

  Scotty.scotty port $ do
    Scotty.get "" $ do
      Scotty.html $ "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

    Scotty.get "/oauth2callback" $ do
      code <- Scotty.param "code"
      eToken <- liftIO $ getAccessToken flow code
      either (Scotty.text . cs) (Scotty.text . cs) eToken

    Scotty.get "/login" $ do
      url <- liftIO $ getAuthorizationURL flow
      Scotty.redirect $ cs url

    Scotty.post "/api" $ do
      r <- Scotty.body >>= handleRPC
      case r of
        Just v -> do
          Scotty.setHeader "Content-Type" "application/json"
          Scotty.raw v
        Nothing -> Scotty.raise "RPC error!"