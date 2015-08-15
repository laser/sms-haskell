{-# LANGUAGE OverloadedStrings #-}

module Controller (
  handleIndex,
  handleLogin,
  handleOAuthCallback,
  handleRPC
) where

import Control.Exception (toException, SomeException(..))
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE, ExceptT)
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans (ActionT, ScottyT, get, post, html, redirect, setHeader, raw, body, raise, param, text)

import Google.OAuth2.AuthFlow (getAuthorizationURL, getAccessToken)
import qualified Google.OAuth2.UserInfo as UI (get)
import RPC (dispatch)
import Persistence (login)
import Types

handleIndex  :: ActionT TL.Text (ExceptT SomeException IO) ()
handleIndex = html $ "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

handleLogin :: OAuth2WebFlow -> ActionT TL.Text (ExceptT SomeException IO) ()
handleLogin flow = do
  url <- liftIO $ getAuthorizationURL flow
  redirect $ cs url

handleOAuthCallback :: OAuth2WebFlow -> ActionT TL.Text (ExceptT SomeException IO) ()
handleOAuthCallback flow = do
  code  <- param "code"
  token <- lift $ getAccessToken flow code
  info  <- lift $ UI.get token

  case info of
    GoogleUserInfo { userId = uid, userName = Just name, userEmail = Just email } -> do
      lift $ login token uid email name
      text "Success! User logged in."
    _ -> lift . throwE . toException $ WAEError "Error: Can't log in without Google username, id, and email address."

handleRPC :: ActionT TL.Text (ExceptT SomeException IO) ()
handleRPC = do
  result <- body >>= dispatch
  case result of
    Just result' -> do
      setHeader "Content-Type" "application/json"
      raw result'
    Nothing -> lift . throwE . toException $ WAEError "RPC Error!"