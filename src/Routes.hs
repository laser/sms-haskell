{-# LANGUAGE OverloadedStrings #-}

module Routes where

import qualified Data.Text.Lazy as TL
import Web.Scotty (get, post)
import Web.Scotty.Trans (ScottyT)

import Types (OAuth2WebFlow(..))
import Controller (handleIndex, handleLogin, handleOAuthCallback, handleRPC)

routes :: OAuth2WebFlow -> ScottyT TL.Text IO ()
routes flow = do
  get "" handleIndex
  get "/login" (handleLogin flow)
  get "/oauth2callback" (handleOAuthCallback flow)
  post "/rpc" handleRPC