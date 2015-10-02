module ProjectService where


import           Control.Error.Util         (hoistEither)
import           Control.Exception          (SomeException (..))
import           Control.Monad              (forM_)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Time.Clock.POSIX      (getPOSIXTime)

import qualified Persistence                as P
import Barrister.IDL.Structs.UserProject as IUP
import Barrister.IDL.Structs.Project as IP
import Barrister.IDL.Structs.User as IU
import Barrister.IDL.Enums as E
import Database.Tables.Login as DBL
import Database.Tables.User as DBU
import Database.Tables.ProjectAccess as DBPA

fakeUserProject = IUP.UserProject
  { IUP.project_id = 1
  , IUP.name = "Great Project"
  , IUP.access_type = OWNER
  }

fakeProject = IP.Project
  { IP.project_id = 1
  , IP.name = "Great Project"
  }

fakeUser = IU.User
  { IU.user_id = "1234"
  , IU.email = Just "hello@example.com"
  , IU.name = "heywood"
  , IU.date_created = 12345
  , IU.default_language = Just E.EN_US
  , IU.default_gps_format = Just E.DECIMAL
  , IU.default_measurement_system = Just E.IMPERIAL
  , IU.default_google_map_type = Just E.SATELLITE
  , IU.needs_to_update_settings = Nothing
  }

login :: String -> String -> String -> String -> ExceptT SomeException IO String
login token userId email name = do
  let
    upsertUser = do
      u <- P.getUserById userId
      case u of
        Just (DBU.User uid email' name' _ lang gps meas gmt) ->
          P.updateUser uid (Just email) (Just name) lang gps meas gmt
        Nothing ->
          P.insertUser userId (Just email) (Just name) Nothing Nothing Nothing E.SATELLITE

    linkProjectAccess = do
      pas <- P.getProjectAccessByEmail email
      forM_ pas (\(DBPA.ProjectAccess { DBPA.project_access_id = paid, DBPA.access_type = at }) -> P.updateProjectAccess paid (Just email) (Just userId) at)

    recordLogin = do
      l <- P.getLoginByUserId userId
      utcms <- liftIO $ getPOSIXTime >>= return . (+86400000) . (*1000) . round
      case l of
        Just DBL.Login { DBL.login_id = lid, DBL.user_id = Just uid } ->
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
getUserSettings :: String -> ExceptT SomeException IO IU.User
getUserSettings token = return fakeUser

-- Required "accessTokenId" :+: ())
getUserProjects :: String -> ExceptT SomeException IO [IUP.UserProject]
getUserProjects token = return [fakeUserProject]

-- Required "accessTokenId" :+: Required "project_id" :+: ())
deleteProject :: String -> String -> ExceptT SomeException IO Bool
deleteProject token projectId = return True
