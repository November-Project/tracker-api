module Handler.FacebookSessions (postFacebookSessionsR) where

import Import
import Helpers.Crypto
import Helpers.Facebook
import Control.Monad (mzero)
import Data.Aeson ((.:?))

postFacebookSessionsR :: Handler Value
postFacebookSessionsR = do
  s <- requireJsonBody :: Handler FacebookAuth
  result <- verifyFacebookToken $ token s
  case result of
    Nothing -> permissionDenied "Facebook"
    Just fid -> do
      mu <- getUserWithFacebookId fid
      createSession s =<< maybe (createOrUpdateFacebookUser $ token s) (\(Entity uid _) -> return uid) mu
  where
    createSession s uid = do
      st <- liftIO $ getRandomToken 32
      _ <- runDB $ insert $ Session uid st (deviceInfo s) False
      sendResponseStatus status200 $ object ["token" .= st]

data FacebookAuth = FacebookAuth { token :: String, deviceInfo :: Maybe Text }

instance FromJSON FacebookAuth where
  parseJSON (Object o) = FacebookAuth
    <$> o .: "token"
    <*> o .:? "device_info"

  parseJSON _ = mzero

