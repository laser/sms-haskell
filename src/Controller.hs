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
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.Aeson                 (decode)
import           Data.String.Conversions    (cs)
import qualified Data.Text.Lazy             as TL
import           Text.Printf                (printf)
import           Web.Scotty.Trans           (ActionT, ScottyT, body, get, html,
                                             param, post, raise, raw, redirect,
                                             setHeader, text)

import           Config                     (Config (..), ServerConfig (..))
import           Google.OAuth2.AuthFlow     (getAccessToken,
                                             getAuthorizationURL)
import qualified Google.OAuth2.UserInfo     as UI (get)
import           Persistence                (login)
import           RPC                        (dispatch)
import           Types

handleIndex  :: ActionT TL.Text (ReaderT Config (ExceptT SomeException IO)) ()
handleIndex = html $ "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

handleLogin :: ActionT TL.Text (ReaderT Config (ExceptT SomeException IO)) ()
handleLogin = do
  cfg <- lift $ ask
  url <- liftIO . getAuthorizationURL $ oauthConfig cfg
  redirect $ cs url

handleOAuthCallback :: ActionT TL.Text (ReaderT Config (ExceptT SomeException IO)) ()
handleOAuthCallback = do
  cfg <- lift $ ask
  code  <- param "code"
  token <- lift . lift $ getAccessToken (oauthConfig cfg) code
  info  <- lift . lift $ UI.get token

  case info of
    GoogleUserInfo { userId = uid, userName = Just name, userEmail = Just email } -> do
      lift . lift $ login token uid email name
      text "Success! User logged in."
    _ -> lift . lift . throwE . toException $ WAEError "Error: Can't log in without Google username, id, and email address."

-- this is dumb: all of the following function should be rewritten
-- to read the sms.json from the environment

handleRPC :: ActionT TL.Text (ReaderT Config (ExceptT SomeException IO)) ()
handleRPC = do
  x <- body
  case decode x of
    Just Request { method = "barrister-idl", cid = cid', version = version' } -> do
      contents <- liftIO $ readFile "./sms.json"
      setHeader "Content-Type" "application/json"
      raw . cs $ mkRawResponse cid' version' contents
    Just _ -> do
      result <- liftIO $ dispatch x
      case result of
        Just result' -> do
          setHeader "Content-Type" "application/json"
          raw result'
        Nothing -> lift . lift . throwE . toException $ WAEError "RPC Error1!"
    Nothing -> lift . lift . throwE . toException $ WAEError "RPC Error2!"
  where mkRawResponse :: String -> String -> String -> String
        mkRawResponse cid version result = printf "{ \"id\": \"%s\", \"version\": \"%s\", \"result\": %s }" cid version result
