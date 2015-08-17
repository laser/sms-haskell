{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.Text.Lazy             as TL
import           Web.Scotty.Trans           (ScottyT, get, param, post, text)

import           Controller                 (handleIndex, handleLogin,
                                             handleOAuthCallback, handleRPC)
import           Types                      (OAuth2WebFlow (..))

routes :: OAuth2WebFlow -> ScottyT TL.Text (ExceptT SomeException IO) ()
routes flow = do
  get "" handleIndex
  get "/login" (handleLogin flow)
  get "/oauth2callback" (handleOAuthCallback flow)
  post "/rpc" handleRPC
