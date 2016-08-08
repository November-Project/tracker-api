module Handler.FacebookSessions (postFacebookSessionsR) where

import Import
import Helpers.Crypto
import Helpers.Facebook
import Data.Aeson ((.:?))

postFacebookSessionsR :: Handler Value
postFacebookSessionsR = do
  s <- requireJsonBody :: Handler FacebookAuth
  createSession s =<< createOrUpdateFacebookUser (token s)
  where
    createSession s uid = do
      st <- liftIO $ getRandomToken 32
      _ <- runDB $ insert $ Session uid st (deviceInfo s) False
      sendResponseStatus status201 $ object ["token" .= st]

data FacebookAuth = FacebookAuth { token :: String, deviceInfo :: Maybe Text }

instance FromJSON FacebookAuth where
  parseJSON (Object o) = FacebookAuth
    <$> o .: "token"
    <*> o .:? "device_info"

  parseJSON _ = mzero

