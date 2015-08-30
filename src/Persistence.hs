{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Persistence where

import           Control.Exception                  (SomeException, throw)
import           Control.Monad.Trans.Except         (ExceptT)
import           Data.ByteString.Char8              (unpack)
import           Data.Int                           (Int64)
import           Data.Maybe                         (listToMaybe)

import           Control.Error                      (syncIO)
import qualified Database.MySQL.Base.Types          as M
import qualified Database.MySQL.Simple              as M
import qualified Database.MySQL.Simple.Param        as M
import qualified Database.MySQL.Simple.QueryParams  as M
import qualified Database.MySQL.Simple.QueryResults as M
import qualified Database.MySQL.Simple.Result       as M
import qualified Database.MySQL.Simple.Types        as M

import           Types

instance M.Result Language where
  convert field Nothing = throw $ M.UnexpectedNull (show (M.fieldType field))
                                                   "Language"
                                                   "UnexpectedNull"
  convert field (Just value)
    | M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
      case unpack value of
        "EN_US" -> EN_US
        "FR_FR" -> FR_FR
        "ES_LA" -> ES_LA
        _       -> throw $ M.ConversionFailed (show (M.fieldType field))
                                              "Language"
                                              "UnexpectedNull"
    | otherwise = throw $ M.Incompatible (show (M.fieldType field))
                                         "Language"
                                         "UnexpectedNull"

instance M.Result MeasurementSystem where
  convert field Nothing =
    throw $ M.UnexpectedNull (show (M.fieldType field))
                             "MeasurementSystem"
                             "UnexpectedNull"
  convert field (Just value)
    | M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
      case unpack value of
        "METRIC" -> METRIC
        "IMPERIAL" -> IMPERIAL
        _       -> throw $ M.ConversionFailed (show (M.fieldType field))
                                              "MeasurementSystem"
                                              "UnexpectedNull"
    | otherwise = throw $ M.Incompatible (show (M.fieldType field))
                                         "MeasurementSystem"
                                         "UnexpectedNull"

instance M.Result GoogleMapType where
  convert field Nothing = throw $ M.UnexpectedNull (show (M.fieldType field))
                                                   "GoogleMapType"
                                                   "UnexpectedNull"
  convert field (Just value)
    | M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
      case unpack value of
        "ROADMAP" -> ROADMAP
        "SATELLITE" -> SATELLITE
        "HYBRID" -> HYBRID
        "TERRAIN" -> TERRAIN
        _       -> throw $ M.ConversionFailed (show (M.fieldType field))
                                              "GoogleMapType"
                                              "UnexpectedNull"
    | otherwise = throw $ M.Incompatible (show (M.fieldType field))
                                         "GoogleMapType"
                                         "UnexpectedNull"

instance M.Result GPSFormat where
  convert field Nothing = throw $ M.UnexpectedNull (show (M.fieldType field))
                                                   "GPSFormat"
                                                   "UnexpectedNull"
  convert field (Just value)
    | M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
      case unpack value of
        "DEGREE" -> DEGREE
        "DECIMAL" -> DECIMAL
        "UTMWGS84" -> UTMWGS84
        _       -> throw $ M.ConversionFailed (show (M.fieldType field))
                                              "GPSFormat"
                                              "UnexpectedNull"
    | otherwise = throw $ M.Incompatible (show (M.fieldType field))
                                         "GPSFormat"
                                         "UnexpectedNull"

instance M.Result AccessType where
  convert field Nothing = throw $ M.UnexpectedNull (show (M.fieldType field))
                                                   "AccessType"
                                                   "UnexpectedNull"
  convert field (Just value)
    | M.fieldType field `elem` [M.VarChar, M.VarString, M.String] =
      case unpack value of
        "OWNER" -> OWNER
        "COLLABORATOR" -> COLLABORATOR
        "READONLY" -> READONLY
        "PUBLIC" -> PUBLIC
        _       -> throw $ M.ConversionFailed (show (M.fieldType field))
                                              "AccessType"
                                              "UnexpectedNull"
    | otherwise = throw $ M.Incompatible (show (M.fieldType field))
                                         "AccessType"
                                         "UnexpectedNull"

instance M.QueryResults User where
  convertResults [fa, fb, fc, fd, fe, ff, fg, fh]
                 [va, vb, vc, vd, ve, vf, vg, vh] = User _userId
                                                         _email
                                                         _name
                                                         _dateCreated
                                                         _defaultLanguage
                                                         _defaultGPSFormat
                                                         _defaultMeasurementSystem
                                                         _defaultGoogleMapType
    where !_userId = M.convert fa va
          !_email = M.convert fb vb
          !_name = M.convert fc vc
          !_dateCreated = M.convert fd vd
          !_defaultLanguage = M.convert fe ve
          !_defaultGPSFormat = M.convert ff vf
          !_defaultMeasurementSystem = M.convert fg vg
          !_defaultGoogleMapType = M.convert fh vh
  convertResults fs vs = M.convertError fs vs 8

instance M.QueryResults ProjectAccess where
  convertResults [fa, fb, fc, fd, fe]
                 [va, vb, vc, vd, ve] = ProjectAccess _projectAccessId
                                                      _projectId
                                                      _userId
                                                      _email
                                                      _accessType
    where !_projectAccessId = M.convert fa va
          !_projectId = M.convert fb vb
          !_userId = M.convert fc vc
          !_email = M.convert fd vd
          !_accessType = M.convert fe ve
  convertResults fs vs = M.convertError fs vs 5

instance M.QueryResults Login where
  convertResults [fa, fb, fc, fd]
                 [va, vb, vc, vd] = Login _loginId
                                          _accessToken
                                          _userId
                                          _expiryTime
    where !_loginId = M.convert fa va
          !_accessToken = M.convert fb vb
          !_userId = M.convert fc vc
          !_expiryTime = M.convert fd vd
  convertResults fs vs = M.convertError fs vs 4

instance M.Param GoogleMapType where
  render = M.render . show

instance M.Param MeasurementSystem where
  render = M.render . show

instance M.Param Language where
  render = M.render . show

instance M.Param GPSFormat where
  render = M.render . show

instance M.Param AccessType where
  render = M.render . show

dbConfig :: M.ConnectInfo
dbConfig = M.defaultConnectInfo { M.connectDatabase = "sms" }

withConnection :: (M.Connection -> IO a) -> IO a
withConnection f = do
  con <- M.connect dbConfig
  res <- f con
  M.close con
  return res

query1 ::
  (M.QueryParams q, M.QueryResults a) => M.Connection
  -> M.Query
  -> q
  -> IO (Maybe a)
query1 c q ps = M.query c q ps >>= return . listToMaybe

getUserById ::
  String
  -> ExceptT SomeException IO (Maybe User)
getUserById userId = syncIO . withConnection $ \conn -> do
   query1 conn
     " SELECT user_id, email, name, date_created, default_language, \
     \        default_gps_format, default_measurement_system, \
     \        default_google_map_type \
     \ FROM `users` \
     \ WHERE user_id=(?) "
     [userId]

insertUser ::
  String
  -> Maybe String
  -> Maybe String
  -> Maybe Language
  -> Maybe GPSFormat
  -> Maybe MeasurementSystem
  -> GoogleMapType
  -> ExceptT SomeException IO Int64
insertUser userId email name lang gps meas mtype = syncIO . withConnection $ \conn -> do
  M.execute conn
     " INSERT INTO `users` (user_id, email, name, date_created, default_language, \
     \                     default_gps_format, default_measurement_system, \
     \                     default_google_map_type) \
     \ VALUES (?, ?, ?, NOW(), ?, ?, ?, ?)"
     (userId, email, name, lang, gps, meas, mtype)

updateUser ::
  String
  -> Maybe String
  -> Maybe String
  -> Maybe Language
  -> Maybe GPSFormat
  -> Maybe MeasurementSystem
  -> GoogleMapType
  -> ExceptT SomeException IO Int64
updateUser userId email name lang gps meas gmt = syncIO . withConnection $ \conn ->
  M.execute conn
     " UPDATE `users` \
     \ SET email=?, \
     \     name=?, \
     \     default_language=?, \
     \     default_gps_format=?, \
     \     default_measurement_system=?, \
     \     default_google_map_type=? \
     \ WHERE user_id=? "
     (email, name, lang, gps, meas, gmt, userId)

getProjectAccessByEmail ::
  String
  -> ExceptT SomeException IO [ProjectAccess]
getProjectAccessByEmail email = syncIO . withConnection $ \conn -> do
   M.query conn
     " SELECT project_access_id, project_id, user_id, email, access_type \
     \ FROM `project_access` \
     \ WHERE email=(?) "
     [email]

updateProjectAccess ::
  Int
  -> Maybe String
  -> Maybe String
  -> Maybe AccessType
  -> ExceptT SomeException IO Int64
updateProjectAccess projectAccessId userId email accessType = syncIO . withConnection $ \conn ->
  M.execute conn
     " UPDATE `project_access` \
     \ SET user_id=?, \
     \     email=?, \
     \     access_type=? \
     \ WHERE project_access_id=? "
     (userId, email, accessType, projectAccessId)

getLoginByUserId ::
  String
  -> ExceptT SomeException IO (Maybe Login)
getLoginByUserId userId = syncIO . withConnection $ \conn ->
  query1 conn
    " SELECT login_id, access_token, user_id, expiry_time \
    \ FROM `logins` \
    \ WHERE user_id=? "
    [userId]

updateLogin ::
  Int
  -> String
  -> String
  -> Int64
  -> ExceptT SomeException IO Int64
updateLogin loginId accessToken userId expiryTime = syncIO . withConnection $ \conn ->
  M.execute conn
    " UPDATE `logins` \
    \ SET access_token=?, \
    \     user_id=?, \
    \     expiry_time=? \
    \ WHERE login_id=? "
    (accessToken, userId, expiryTime, loginId)

insertLogin ::
  String
  -> String
  -> Int64
  -> ExceptT SomeException IO Int64
insertLogin accessToken userId expiryTime = syncIO . withConnection $ \conn ->
  M.execute conn
     " INSERT INTO `logins` (access_token, user_id, expiry_time) \
     \ VALUES (?, ?, ?)"
     (accessToken, userId, expiryTime)

recordLogin ::
  String
  -> String
  -> ExceptT SomeException IO Int64
recordLogin token userId = syncIO . withConnection $ \conn ->
  M.execute conn
    " INSERT INTO `logins` (access_token, user_id, expiry_time) \
    \ VALUES (?, ?, NOW()+INTERVAL 1 DAY) \
    \ ON DUPLICATE KEY UPDATE access_token=(?), expiry_time=NOW()+INTERVAL 1 DAY "
    (token, userId, token)
