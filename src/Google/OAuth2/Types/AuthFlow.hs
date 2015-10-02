module Google.OAuth2.Types.AuthFlow where

data AuthFlow = WebFlow
  { scope        :: String
  , authURI      :: String
  , tokenURI     :: String
  , responseType :: String
  , redirectURI  :: String
  , clientId     :: String
  , clientSecret :: String
  } deriving (Show)
