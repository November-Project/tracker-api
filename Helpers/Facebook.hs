module Helpers.Facebook (verifyFacebookToken, getUserWithFacebookId, createOrUpdateFacebookUser) where

import Import
import System.Environment (getEnv)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode)
import Database.Persist.Sql (toSqlKey)

verifyFacebookToken :: String -> Handler (Maybe Text)
verifyFacebookToken t = do
  aid <- liftIO $ getEnv "FACEBOOK_APP_ID"
  s <- liftIO $ getEnv "FACEBOOK_SECRET"
  result <- liftIO $ simpleHttp $ concat [tokenVerifyURL, "?input_token=", t, "&access_token=", aid, "|", s]

  maybe (invalidArgs ["token"]) (\fa -> do
    if isValid fa
      then return $ Just $ userId fa
      else return Nothing) $ decode result
  where
    tokenVerifyURL = "https://graph.facebook.com/v2.5/debug_token"

data FacebookAuthResponse = FacebookAuthResponse { isValid :: Bool, userId :: Text }

instance FromJSON FacebookAuthResponse where
  parseJSON (Object o) = FacebookAuthResponse
    <$> ((o .: "data") >>= (.: "is_valid"))
    <*> ((o .: "data") >>= (.: "user_id"))

  parseJSON _ = mzero

getUserWithFacebookId :: Text -> Handler (Maybe (Entity User))
getUserWithFacebookId fid = runDB $ getBy $ UniqueUserFacebookId $ Just fid 

createOrUpdateFacebookUser :: String -> Handler UserId
createOrUpdateFacebookUser t = do
  result <- liftIO $ simpleHttp $ concat [profileURL, "?access_token=", t]
  
  maybe (invalidArgs ["token2"]) (\fu -> do
    muEmail <- runDB $ getBy $ UniqueUserEmail $ email fu
    muFacebook <- runDB $ getBy $ UniqueUserFacebookId $ Just $ facebookId fu
    case (muEmail, muFacebook) of
      (Just uEmail, _) -> updateUser uEmail fu
      (_, Just (Entity uid _)) -> return uid
      (_, _) -> createUser fu
    ) $ decode result
  where
    profileURL = "https://graph.facebook.com/me"
    createUser u = runDB $ insert $ User (name u) (email u) Nothing (gender u) (toSqlKey 1) (Just $ "http://graph.facebook.com/" ++ facebookId u  ++ "/picture") (Just $ facebookId u) False Nothing (verified u) Nothing False Nothing True
    updateUser (Entity uid _) fu = do
      runDB $ update uid [UserName =. name fu, UserFacebookId =. Just (facebookId fu)]
      return uid

data FacebookUser = FacebookUser
  { facebookId :: Text
  , name :: Text
  , email :: Text
  , gender :: Text
  , verified :: Bool
  }

instance FromJSON FacebookUser where
  parseJSON (Object o) = FacebookUser
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "email"
    <*> o .: "gender"
    <*> o .: "verified"

  parseJSON _ = mzero
