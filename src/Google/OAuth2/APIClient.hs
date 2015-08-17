module Google.OAuth2.APIClient (
  issueRequest
) where

import           Control.Monad.Trans        (liftIO)
import           Network.HTTP.Conduit       (HttpException, Request, httpLbs,
                                             newManager, responseBody,
                                             tlsManagerSettings)

import           Control.Error              (syncIO)
import           Control.Exception          (SomeException)
import           Control.Monad.Trans.Except (ExceptT)

import qualified Data.ByteString.Lazy       as BL

issueRequest :: Request -> ExceptT SomeException IO BL.ByteString
issueRequest request = syncIO $ do
  manager  <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ responseBody response
