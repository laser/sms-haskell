{-# LANGUAGE OverloadedStrings #-}

module RPC (dispatch) where


import           Control.Error.Util         (hoistEither)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Trans        (liftIO)
import           Network.JsonRpc.Server     ((:+:) (..), Method (..),
                                             Parameter (..), RpcResult (..),
                                             call, rpcError, toMethod)

import           Control.Exception          (SomeException (..))
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

import qualified Data.ByteString.Lazy       as BL

import           Types

fakeUserProject = UserProject { userproject_project_id = 1
                              , userproject_name = "Great Project"
                              , userproject_access_type = OWNER }

fakeProject = Project { project_project_id = 1
                      , project_name = "Great Project" }

fakeUser = User { user_user_id = "1234"
                , user_email = Just "hello@example.com"
                , user_name = "heywood"
                , user_date_created = 12345
                , user_default_language = Just EN_US
                , user_default_gps_format = Just DECIMAL
                , user_default_measurement_system = Just IMPERIAL
                , user_default_google_map_type = Just SATELLITE
                , user_needs_to_update_settings = Nothing }

fakeUpdateUserSettingsResult = True

fakeDeleteProjectResult = True

addProject :: String -> String -> ExceptT SomeException IO Project
addProject token name = do
  liftIO $ print "howdy"
  hoistEither $ Right fakeProject

adapter :: ExceptT SomeException IO a -> RpcResult IO a
adapter et = do
  e <- liftIO $ runExceptT et
  case e of
    Left _ -> throwE $ rpcError 12345 "bad stuff happened"
    Right x -> return x

f1 :: Method IO
f1 = toMethod "ProjectService.add_project" f (Required "accessTokenId" :+: Required "projectName" :+: ())
  where f token name = adapter $ addProject token name

f2 :: Monad m => Method m
f2 = toMethod "ProjectService.update_user_settings" f (Required "accessTokenId" :+: Required "defaultLanguage" :+: Required "defaultGpsFormat" :+: Required "defaultMeasurementSystem" :+: Required "defaultGoogleMapType" :+: ())
  where f :: Monad m => String -> String -> String -> String -> String -> RpcResult m Bool
        f token lang gps meas mtype = return True

f3 :: Monad m => Method m
f3 = toMethod "ProjectService.get_user_settings" f (Required "accessTokenId" :+: ())
  where f :: Monad m => String -> RpcResult m User
        f token = return fakeUser

f4 :: Monad m => Method m
f4 = toMethod "ProjectService.get_projects" f (Required "accessTokenId" :+: ())
  where f :: Monad m => String -> RpcResult m [UserProject]
        f token = return [fakeUserProject]

f5 :: Monad m => Method m
f5 = toMethod "ProjectService.delete_project" f (Required "accessTokenId" :+: Required "project_id" :+: ())
  where f :: Monad m => String -> Integer -> RpcResult m Bool
        f token pid = return $ True

dispatch :: BL.ByteString -> IO (Maybe BL.ByteString)
dispatch = call [f1, f2, f3, f4, f5]
