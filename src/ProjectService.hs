module ProjectService where


import           Control.Error.Util         (hoistEither)
import           Control.Exception          (SomeException (..))
import           Control.Monad              (forM_)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import qualified Persistence                as P
import qualified Types as Core
import Barrister.IDL.Structs.UserProject as IDL.UserProject
import Barrister.IDL.Structs.Project as IDL.Project
import Barrister.IDL.Structs.User as IDL.User
import Barrister.IDL.Enums as IDL.Enums

fakeUserProject = IDL.UserProject.UserProject
  { IDL.UserProject.project_id = 1
  , IDL.UserProject.name = "Great Project"
  , IDL.UserProject.access_type = OWNER
  }

fakeProject = IDL.Project.Project
  { IDL.Project.project_id = 1
  , IDL.Project.name = "Great Project"
  }

fakeUser = IDL.User.User
  { IDL.User.user_id = "1234"
  , IDL.User.email = Just "hello@example.com"
  , IDL.User.name = "heywood"
  , IDL.User.date_created = 12345
  , IDL.User.default_language = Just IDL.Enums.EN_US
  , IDL.User.default_gps_format = Just IDL.Enums.DECIMAL
  , IDL.User.default_measurement_system = Just IDL.Enums.IMPERIAL
  , IDL.User.default_google_map_type = Just IDL.Enums.SATELLITE
  , IDL.User.needs_to_update_settings = Nothing
  }

login :: String -> String -> String -> String -> ExceptT SomeException IO String
login token userId email name = do
  let
    upsertUser = do
      u <- P.getUserById userId
      case u of
        Just (Core.User uid email' name' _ lang gps meas gmt) ->
          P.updateUser uid (Just email) (Just name) lang gps meas gmt
        Nothing ->
          P.insertUser userId (Just email) (Just name) Nothing Nothing Nothing Core.SATELLITE

    linkProjectAccess = do
      pas <- P.getProjectAccessByEmail email
      forM_ pas (\(Core.ProjectAccess paid _ _ _ at) -> P.updateProjectAccess paid (Just email) (Just userId) at)

    recordLogin = do
      l <- P.getLoginByUserId userId
      utcms <- liftIO $ getPOSIXTime >>= return . (+86400000) . (*1000) . round
      case l of
        Just (Core.Login lid _ uid _) ->
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
getUserProjects :: String -> ExceptT SomeException IO [IDL.UserProject.UserProject]
getUserProjects token = return [fakeUserProject]

-- Required "accessTokenId" :+: Required "project_id" :+: ())
deleteProject :: String -> String -> ExceptT SomeException IO Bool
deleteProject token projectId = return True
