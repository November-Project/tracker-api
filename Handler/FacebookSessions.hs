module Handler.FacebookSessions (postFacebookSessionsR) where

import Import
import Helpers.Crypto
import System.Environment (getEnv)
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad (mzero)
import Data.Aeson (decode, (.:?))

postFacebookSessionsR :: Handler Value
postFacebookSessionsR = do
  s <- requireJsonBody :: Handler FacebookAuth
  aid <- liftIO $ getEnv "FACEBOOK_APP_ID"
  t <- liftIO $ getEnv "FACEBOOK_SECRET"
  result <- liftIO $ simpleHttp $ foldl (++) "" [tokenVerifyURL, "?input_token=", token s, "&access_token=", aid, "|", t]
  maybe (invalidArgs ["token"]) (\fa -> do
    if not $ isValid fa
      then permissionDenied "Facebook"
      else do
        Entity uid _ <- getUserFromFacebookId $ userId fa
        st <- liftIO $ getRandomToken 32
        _ <- runDB $ insert $ Session uid st $ deviceInfo s
        sendResponseStatus status200 st) $ decode result
  where
    tokenVerifyURL = "https://graph.facebook.com/debug_token"
    getUserFromFacebookId fid = do
      users <- runDB $ selectList [UserFacebookId ==. Just fid] []
      case users of
        [] -> notFound
        x:_ -> return x

data FacebookAuth = FacebookAuth { token :: String, deviceInfo :: Maybe Text }

instance FromJSON FacebookAuth where
  parseJSON (Object o) = FacebookAuth
    <$> o .: "token"
    <*> o .:? "device_info"

  parseJSON _ = mzero

data FacebookAuthResponse = FacebookAuthResponse { isValid :: Bool, userId :: Int }

instance FromJSON FacebookAuthResponse where
  parseJSON (Object o) = FacebookAuthResponse
    <$> ((o .: "data") >>= (.: "is_valid"))
    <*> ((o .: "data") >>= (.: "user_id"))

  parseJSON _ = mzero
