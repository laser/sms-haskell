{-# LANGUAGE OverloadedStrings #-}

module Controller where

import           Control.Exception          (SomeException (..), toException)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Control.Monad.Trans.Reader (ReaderT, ask, asks)
import           Text.Printf                (printf)

import           Data.Aeson                 (decode)
import           Data.String.Conversions    (cs)
import           Data.Text.Lazy             (Text)
import           Web.Scotty.Trans           (ActionT, body, get, html,
                                             param, post, raise, raw, redirect,
                                             setHeader, text)

import           Config                     as C
import qualified Google.OAuth2.AuthFlow     as AF
import qualified Google.OAuth2.UserInfo     as UI
import qualified ProjectService             as SVC
import qualified RPC                        as RPC
import           Types

handleIndex  :: ActionT Text (ReaderT C.Config (ExceptT SomeException IO)) ()
handleIndex = html "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

handleLogin :: ActionT Text (ReaderT Config (ExceptT SomeException IO)) ()
handleLogin = do
  cfg <- lift $ asks C.oauthConfig
  url <- liftIO (AF.getAuthorizationURL cfg)
  redirect $ cs url

handleOAuthCallback :: ActionT Text (ReaderT C.Config (ExceptT SomeException IO)) ()
handleOAuthCallback = do
  cfg   <- lift $ asks C.oauthConfig
  code  <- param "code"
  token <- lift . lift $ AF.getAccessToken cfg code
  info  <- lift . lift $ UI.get token

  case info of
    GoogleUserInfo { userId = uid, userName = Just name, userEmail = Just email } -> do
      lift . lift $ SVC.login token uid email name
      text "Success! User logged in."
    _ -> lift . lift . throwE . toException $ WAEError "Error: Can't log in without Google username, id, and email address."

-- this is dumb: all of the following function should be rewritten
-- to read the sms.json from the environment

handleRPC :: ActionT Text (ReaderT Config (ExceptT SomeException IO)) ()
handleRPC = do
  x <- body
  case decode x of
    Just Request { method = "barrister-idl", cid = cid', version = version' } -> do
      idl <- lift $ asks (C.idl . C.barristerConfig)
      setHeader "Content-Type" "application/json"
      raw . cs $ mkRawResponse cid' version' idl
    Just _ -> do
      result <- liftIO $ RPC.dispatch x
      case result of
        Just result' -> do
          setHeader "Content-Type" "application/json"
          raw result'
        Nothing -> lift . lift . throwE . toException $ WAEError "RPC Error1!"
    Nothing -> lift . lift . throwE . toException $ WAEError "RPC Error2!"
  where mkRawResponse :: String -> String -> String -> String
        mkRawResponse = printf "{ \"id\": \"%s\", \"version\": \"%s\", \"result\": %s }"
