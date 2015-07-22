{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Environment (getEnvironment)

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty as Scotty

import OAuth2 (OAuth2WebFlow(..), step1GetAuthorizeURL)
import RPCService (handle)

main = do
  env <- getEnvironment
  let port = maybe 3000 read $ lookup "PORT" env
      x = maybe "" id $ lookup "GOOGLE_OAUTH_CLIENT_ID" env
      y = maybe "" id $ lookup "GOOGLE_OAUTH_CLIENT_SECRET" env

  Scotty.scotty port $ do
    Scotty.get "" $ do
      Scotty.html $ "<h1>Welcome</h1><a href=\"/login\">click here to log in</a>"

    Scotty.get "/oauth2callback" $ do
      v <- Scotty.param "code"
      Scotty.text v

    Scotty.get "/login" $ do
      let flow = OAuth2WebFlow { scope = "https://www.googleapis.com/auth/userinfo.profile https://www.googleapis.com/auth/userinfo.email"
                               , redirectURI = "http://localhost:5000/oauth2callback"
                               , authURI = "https://accounts.google.com/o/oauth2/auth"
                               , tokenURI = "https://accounts.google.com/o/oauth2/token"
                               , responseType = "code"
                               , clientId = BC.pack x
                               , clientSecret = BC.pack y }

      case step1GetAuthorizeURL flow of
        Just url -> Scotty.redirect (TL.pack url)
        Nothing -> Scotty.text "you're in trouble"

    Scotty.post "/api" $ do
      Scotty.body >>= handle >>= Scotty.json . show