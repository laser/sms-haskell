module Database.Tables.ProjectAccess where

import Barrister.IDL.Enums (AccessType)

data ProjectAccess = ProjectAccess
  { project_access_id     :: Int
  , project_id :: Int
  , user_id :: Maybe String
  , email      :: Maybe String
  , access_type  :: Maybe AccessType
}