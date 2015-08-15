{-# LANGUAGE OverloadedStrings #-}

module Routing where

import Control.Exception (SomeException)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy as TL
import Web.Scotty (get, post, html, redirect, setHeader, raw, body, raise, param, text, ActionM)
import Web.Scotty.Trans (ActionT, ScottyT)

import Google.OAuth2.AuthFlow (getAuthorizationURL, getAccessToken)
import qualified Google.OAuth2.UserInfo as UI (get)
import RPC (handleRPC)
import Service (login)
import Types (OAuth2WebFlow(..), GoogleUserInfo(..))

routes :: OAuth2WebFlow -> ScottyT TL.Text IO ()
routes flow = do
  get "" handleIndex
  get "/login" (handleLogin flow)
  get "/oauth2callback" (handleOAuthCallback flow)
  post "/api" handleAPI

handleIndex :: ActionM ()
handleIndex = html $ "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

handleLogin :: OAuth2WebFlow -> ActionT TL.Text IO ()
handleLogin flow = do
  url <- liftIO $ getAuthorizationURL flow
  redirect $ cs url

handleOAuthCallback :: OAuth2WebFlow -> ActionT TL.Text IO ()
handleOAuthCallback flow = do
  code   <- param "code"
  eToken <- liftIO . runExceptT $ getAccessToken flow code

  case eToken of
    Left err -> text . cs $ show err
    Right token -> do
      eInfo <- liftIO . runExceptT $ UI.get token
      case eInfo of
        Left err -> text . cs $ show err
        Right GoogleUserInfo { userId = uid, userName = Just name, userEmail = Just email } -> do
          loginResult <- liftIO . runExceptT $ login token uid email name
          case loginResult of
            Left ex -> text . cs $ show ex
            Right token -> text "Success! User logged in."
        Right _ -> text "Error: Can't log in without Google username, id, and email address."

handleAPI :: ActionT TL.Text IO ()
handleAPI = do
  r <- body >>= handleRPC
  case r of
    Just v -> do
      setHeader "Content-Type" "application/json"
      raw v
    Nothing -> raise "RPC error!"