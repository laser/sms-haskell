{-# LANGUAGE OverloadedStrings #-}

module Main (
  main
) where

import Control.Exception (SomeException)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Data.ByteString.Builder (lazyByteString)
import Data.String.Conversions (cs)
import Network.HTTP.Types (Status, status500)
import Network.Wai (Response, responseBuilder)
import System.Environment (getEnv)
import Web.Scotty.Trans (scottyT)

import Routes (routes)
import Types (OAuth2WebFlow(..))

import qualified Temp as T

main :: IO ()
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

  scottyT prt runAction $ routes flow

runAction :: ExceptT SomeException IO Response -> IO Response
runAction et = do
  r <- runExceptT et
  case r of
    Left err -> handleError err
    Right r' -> return r'

handleError :: SomeException -> IO Response
handleError ex = return $ msgBuilder (show ex) status500

msgBuilder :: String -> Status -> Response
msgBuilder msg s =
  responseBuilder s [("Content-Type","text/plain")] . lazyByteString . cs $ msg