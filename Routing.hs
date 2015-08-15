{-# LANGUAGE OverloadedStrings #-}

module Routing where

import qualified Data.Text.Lazy as TL
import Web.Scotty.Trans (ScottyT)

import Controllers (handleIndex, handleLogin, handleOAuthCallback, handleAPI)
import Types (OAuth2WebFlow(..))

routes :: OAuth2WebFlow -> ScottyT TL.Text IO ()
routes flow = do
  get "" handleIndex
  get "/login" (handleLogin flow)
  get "/oauth2callback" (handleOAuthCallback flow)
  post "/api" handleAPI