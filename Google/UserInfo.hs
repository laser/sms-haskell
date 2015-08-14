module Google.UserInfo (
  getAuthorizationURL,
  getAccessToken,
  getUserInfo
) where

import Google.OAuth2.HTTPClient (issueRequest222)

get :: String -> IO (Either String GoogleUserInfo)
get token = do
  request <- parseUrl "https://www.googleapis.com/oauth2/v2/userinfo"

  eBody <- issueRequest request { requestHeaders = [("Authorization", BS.append "Bearer " (cs token))] }

  return $ either (Left . show) (eitherDecode) eBody