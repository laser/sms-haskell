{-# LANGUAGE OverloadedStrings #-}

module Persistence (
  login
) where

import           Control.Error              (syncIO)
import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)
import           Data.Int                   (Int64)
import           Database.MySQL.Simple      (ConnectInfo (..), Connection,
                                             close, connect, defaultConnectInfo,
                                             execute)

dbConfig :: ConnectInfo
dbConfig = defaultConnectInfo { connectDatabase = "sms" }

withConnection :: (Connection -> IO a) -> IO a
withConnection f = do
  con <- connect dbConfig
  res <- f con
  close con
  return res

login :: String -> String -> String -> String -> ExceptT SomeException IO String
login token userId email name =
  upsertUser userId email name
  >> recordLogin token userId
  >> linkProjectAccess userId email
  >> return token

upsertUser :: String -> String -> String -> ExceptT SomeException IO Int64
upsertUser userId email name = syncIO . withConnection $ \conn ->
  execute conn
    " INSERT INTO `users` (user_id, email, name, date_created) \
    \ VALUES (?, ?, ?, NOW()) \
    \ ON DUPLICATE KEY UPDATE email=(?), name=(?) "
    (userId, email, name, email, name)

linkProjectAccess :: String -> String -> ExceptT SomeException IO Int64
linkProjectAccess userId email = syncIO . withConnection $ \conn ->
  execute conn
    " UPDATE `project_access` \
    \ SET user_id = (?) \
    \ WHERE email=(?) "
    (userId, email)

recordLogin :: String -> String -> ExceptT SomeException IO Int64
recordLogin token userId = syncIO . withConnection $ \conn ->
  execute conn
    " INSERT INTO `logins` (access_token, user_id, expiry_time) \
    \ VALUES (?, ?, NOW()+INTERVAL 1 DAY) \
    \ ON DUPLICATE KEY UPDATE access_token=(?), expiry_time=NOW()+INTERVAL 1 DAY "
    (token, userId, token)
