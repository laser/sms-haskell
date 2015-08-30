module ProjectService where


import           Control.Error.Util         (hoistEither)
import           Control.Exception          (SomeException (..))
import           Control.Monad              (forM_)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import qualified Persistence                as P
import           Types

fakeUserProject = UserProject { userproject_project_id = 1
                              , userproject_name = "Great Project"
                              , userproject_access_type = OWNER }

fakeProject = Project { project_project_id = 1
                      , project_name = "Great Project" }

fakeUser = User { user_user_id = "1234"
                , user_email = Just "hello@example.com"
                , user_name = Just "heywood"
                , user_date_created = 12345
                , user_default_language = Just EN_US
                , user_default_gps_format = Just DECIMAL
                , user_default_measurement_system = Just IMPERIAL
                , user_default_google_map_type = SATELLITE }

login :: String -> String -> String -> String -> ExceptT SomeException IO String
login token userId email name = do
  let
    upsertUser = do
      u <- P.getUserById userId
      case u of
        Just (User uid email' name' _ lang gps meas gmt) ->
          P.updateUser uid (Just email) (Just name) lang gps meas gmt
        Nothing ->
          P.insertUser userId (Just email) (Just name) Nothing Nothing Nothing SATELLITE

    linkProjectAccess = do
      pas <- P.getProjectAccessByEmail email
      forM_ pas (\(ProjectAccess paid _ _ _ at) -> P.updateProjectAccess paid (Just email) (Just userId) at)

    recordLogin = do
      l <- P.getLoginByUserId userId
      utcms <- liftIO $ getPOSIXTime >>= return . (+86400000) . (*1000) . round
      case l of
        Just (Login lid _ uid _) ->
          P.updateLogin lid token uid utcms
        Nothing ->
          P.insertLogin token userId utcms

  upsertUser >> linkProjectAccess >> recordLogin >> (return token)

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
