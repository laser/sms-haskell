{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Text.Lazy             (Text)

import           Control.Monad.Trans.Reader (ReaderT)
import           Web.Scotty.Trans           (ScottyT, get, param, post, text)

import           Config
import           Controller
import           Types

routes :: ScottyT Text (ReaderT Config (ExceptT SomeException IO)) ()
routes = do
  get "" handleIndex
  get "/login" handleLogin
  get "/oauth2callback" handleOAuthCallback
  post "/rpc" handleRPC
