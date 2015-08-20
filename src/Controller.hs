{-# LANGUAGE OverloadedStrings #-}

module Controller (
  handleIndex,
  handleLogin,
  handleOAuthCallback,
  handleRPC
) where

import           Control.Exception          (SomeException (..), toException)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Aeson                 (decode)
import           Data.String.Conversions    (cs)
import qualified Data.Text.Lazy             as TL
import           Text.Printf                (printf)
import           Web.Scotty.Trans           (ActionT, ScottyT, body, get, html,
                                             param, post, raise, raw, redirect,
                                             setHeader, text)

import           Google.OAuth2.AuthFlow     (getAccessToken,
                                             getAuthorizationURL)
import qualified Google.OAuth2.UserInfo     as UI (get)
import           Persistence                (login)
import           RPC                        (dispatch)
import           Types

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
  x <- body
  case decode x of
    Just Request { method = "barrister-idl", cid = cid', version = version' } -> do
      contents <- liftIO $ readFile "./sms.json" -- this is dumb: all of this block should be rewritten
      setHeader "Content-Type" "application/json"
      raw . cs $ mkRawResponse cid' version' contents
    Just _ -> do
      result <- dispatch x
      case result of
        Just result' -> do
          setHeader "Content-Type" "application/json"
          raw result'
        Nothing -> lift . throwE . toException $ WAEError "RPC Error1!"
    Nothing -> lift . throwE . toException $ WAEError "RPC Error2!"
  where mkRawResponse :: String -> String -> String -> String
        mkRawResponse cid version result = printf "{ \"id\": \"%s\", \"version\": \"%s\", \"result\": %s }" cid version result
