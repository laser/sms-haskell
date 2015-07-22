module OAuth2 where

import Data.String.Conversions (cs)
import Network.URL (exportURL, importURL, add_param)

import qualified Data.ByteString.Lazy.Char8 as BLC

data OAuth2WebFlow = OAuth2WebFlow { scope :: BLC.ByteString
                                   , redirectURI :: BLC.ByteString
                                   , authURI :: BLC.ByteString
                                   , tokenURI :: BLC.ByteString
                                   , responseType :: BLC.ByteString
                                   , clientId :: BLC.ByteString
                                   , clientSecret :: BLC.ByteString } deriving (Show)

step1GetAuthorizeURL :: OAuth2WebFlow -> Maybe String
step1GetAuthorizeURL flow = do
  parsed <- importURL . cs $ authURI flow

  let params = [ ("scope", (scope flow))
               , ("client_id", (clientId flow))
               , ("redirect_uri", (redirectURI flow))
               , ("response_type", (responseType flow)) ]

  return $ exportURL $ foldl (\acc (prop, val) -> add_param acc (prop, cs val)) parsed params