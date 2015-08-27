{-# LANGUAGE OverloadedStrings #-}

module RPC (dispatch) where


import           Control.Exception          (SomeException (..))
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.ByteString.Lazy       (ByteString)

import           Control.Error.Util         (hoistEither)
import           Network.JsonRpc.Server     ((:+:) (..), Method,
                                             Parameter (Required), RpcResult,
                                             call, rpcError, toMethod)

import           ProjectService
import           Types


-- | Runs a failable computation and transmutes error
-- messages to the appropriate JSON-RPC error code.
adapter ::
  ExceptT SomeException IO a  -- ^ A failable IO computation with error message
  -> RpcResult IO a           -- ^ RPC representation of an IO computation
adapter et = do
  e <- liftIO $ runExceptT et
  case e of
    Left _ -> throwE $ rpcError 12345 "blah blah"
    Right x -> return x

-- | Dispatch a JSON-RPC request to the appropriate
-- service. Accepts and returns JSON-encoded ByteStrings.
dispatch ::
  ByteString                -- ^ A JSON-RPC request
  -> IO (Maybe ByteString)  -- ^ A computation resulting in a JSON-encoded RPC response
dispatch = call [ toMethod "ProjectService.add_project" (\a1 a2 -> adapter $ addProject a1 a2) (Required "accessTokenId" :+: Required "projectName" :+: ())
                , toMethod "ProjectService.update_user_settings" (\a1 a2 a3 a4 a5 -> adapter $ updateUserSettings a1 a2 a3 a4 a5) (Required "accessTokenId" :+: Required "defaultLanguage" :+: Required "defaultGpsFormat" :+: Required "defaultMeasurementSystem" :+: Required "defaultGoogleMapType" :+: ())
                , toMethod "ProjectService.get_user_settings" (\a1 -> adapter $ getUserSettings a1) (Required "accessTokenId" :+: ())
                , toMethod "ProjectService.get_projects" (\a1 -> adapter $ getUserProjects a1) (Required "accessTokenId" :+: ())
                , toMethod "ProjectService.delete_project" (\a1 a2 -> adapter $ deleteProject a1 a2) (Required "accessTokenId" :+: Required "project_id" :+: ())
                ]
