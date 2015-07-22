module OAuth2 where

import Data.ByteString (ByteString(..))
import Data.ByteString.Char8 (unpack)
import Network.URL (exportURL, importURL, add_param)

data OAuth2WebFlow = OAuth2WebFlow { scope :: ByteString
                                   , redirectURI :: ByteString
                                   , authURI :: ByteString
                                   , tokenURI :: ByteString
                                   , responseType :: ByteString
                                   , clientId :: ByteString
                                   , clientSecret :: ByteString } deriving (Show)

step1GetAuthorizeURL :: OAuth2WebFlow -> Maybe String
step1GetAuthorizeURL flow = do
  parsed <- importURL $ unpack $ authURI flow

  let params = [ ("scope", (scope flow))
               , ("client_id", (clientId flow))
               , ("redirect_uri", (redirectURI flow))
               , ("response_type", (responseType flow)) ]

  return $ exportURL $ foldl (\acc (prop, val) -> add_param acc (prop, unpack val)) parsed params