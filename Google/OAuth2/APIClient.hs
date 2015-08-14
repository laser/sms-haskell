module Google.OAuth2.APIClient (
  issueRequest
) where

import Control.Exception (catch)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Conduit (newManager, responseBody, tlsManagerSettings, httpLbs, HttpException, Request)

import qualified Data.ByteString.Lazy as BL

issueRequest :: Request -> IO (Either HttpException BL.ByteString)
issueRequest request = liftIO $ catch (issue request) (return . Left)
  where
    issue r = do
      response <- newManager tlsManagerSettings >>= httpLbs r
      return . Right $ responseBody response