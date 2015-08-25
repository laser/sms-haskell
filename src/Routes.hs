{-# LANGUAGE OverloadedStrings #-}

module Routes where

import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)
import qualified Data.Text.Lazy             as TL
import           Web.Scotty.Trans           (ScottyT, get, param, post, text)

import           Controller                 (handleIndex, handleLogin,
                                             handleOAuthCallback, handleRPC)
import           Types                      (OAuth2WebFlow (..))

import           Config                     (Config (..), ServerConfig (..),
                                             getConfig)
import           Control.Monad.Trans.Reader (ReaderT)

routes :: ScottyT TL.Text (ReaderT Config (ExceptT SomeException IO)) ()
routes = do
  get "" handleIndex
  get "/login" handleLogin
  get "/oauth2callback" handleOAuthCallback
  post "/rpc" handleRPC
