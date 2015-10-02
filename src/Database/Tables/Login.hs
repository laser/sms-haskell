module Database.Tables.Login where

import           Data.Int                (Int64)

data Login = Login
  { login_id     :: Int
  , access_token :: Maybe String
  , user_id      :: Maybe String
  , expiry_time  :: Int64
}