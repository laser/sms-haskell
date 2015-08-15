module Google.OAuth2.APIClient (
  issueRequest
) where

import Control.Monad.Trans (liftIO)
import Network.HTTP.Conduit (newManager, responseBody, tlsManagerSettings, httpLbs, HttpException, Request)

import Control.Error (syncIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Exception (SomeException)

import qualified Data.ByteString.Lazy as BL

issueRequest :: Request -> ExceptT SomeException IO BL.ByteString
issueRequest request = syncIO $ do
  manager  <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ responseBody response