module ProjectService where


import           Control.Error.Util         (hoistEither)
import           Control.Exception          (SomeException (..))
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)

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

addProject :: String -> String -> ExceptT SomeException IO Project
addProject token name = do
  liftIO $ print "howdy"
  hoistEither $ Right fakeProject

-- Required "accessTokenId" :+: Required "defaultLanguage" :+: Required "defaultGpsFormat" :+: Required "defaultMeasurementSystem" :+: Required "defaultGoogleMapType" :+: ()
updateUserSettings :: String -> String -> String -> String -> String -> ExceptT SomeException IO Bool
updateUserSettings token lang gps meas mtype = return True

-- Required "accessTokenId"
getUserSettings :: String -> ExceptT SomeException IO User
getUserSettings token = return fakeUser

-- Required "accessTokenId" :+: ())
getUserProjects :: String -> ExceptT SomeException IO [UserProject]
getUserProjects token = return [fakeUserProject]

-- Required "accessTokenId" :+: Required "project_id" :+: ())
deleteProject :: String -> String -> ExceptT SomeException IO Bool
deleteProject token projectId = return True
