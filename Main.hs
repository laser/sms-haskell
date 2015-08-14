{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.String.Conversions (cs)
import System.Environment (getEnvironment)
import Control.Exception (SomeException)

import qualified Web.Scotty as Scotty

import OAuth2 (GoogleUserInfo(..), OAuth2WebFlow(..), getAuthorizationURL, getAccessToken, getUserInfo)
import RPC (handleRPC)
import Service (login)

main = do
  env <- getEnvironment

  let port = maybe 3000 read $ lookup "PORT" env
      x = maybe "" id $ lookup "SMS_GOOGLE_LOGIN_CLIENT_ID" env
      y = maybe "" id $ lookup "SMS_GOOGLE_LOGIN_CLIENT_SECRET" env
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

      case eToken of
        Left err -> Scotty.text $ cs err
        Right token -> do
          eInfo <- liftIO $ getUserInfo token
          case eInfo of
            Left err -> Scotty.text $ cs err
            Right GoogleUserInfo { userId = uid, userName = Just name, userEmail = Just email } -> do
              loginResult <- liftIO . runExceptT $ Service.login token uid email name
              case loginResult of
                Left ex -> Scotty.text $ cs $ show ex
                Right token -> Scotty.text "Success! User logged in."
            Right _ -> Scotty.text "Error: Can't log in without Google username, id, and email address."

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