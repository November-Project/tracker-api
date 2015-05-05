module Helpers.Facebook (verifyFacebookToken, getUserWithFacebookId, createOrUpdateFacebookUser) where

import Import
import System.Environment (getEnv)
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad (mzero)
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
    tokenVerifyURL = "https://graph.facebook.com/v2.3/debug_token"

data FacebookAuthResponse = FacebookAuthResponse { isValid :: Bool, userId :: Text }

instance FromJSON FacebookAuthResponse where
  parseJSON (Object o) = FacebookAuthResponse
    <$> ((o .: "data") >>= (.: "is_valid"))
    <*> ((o .: "data") >>= (.: "user_id"))

  parseJSON _ = mzero

getUserWithFacebookId :: Text -> Handler (Maybe (Entity User))
getUserWithFacebookId fid = do
  users <- runDB $ selectList [UserFacebookId ==. Just fid] []
  case users of
    [] -> return Nothing
    x:_ -> return $ Just x

createOrUpdateFacebookUser :: String -> Handler UserId
createOrUpdateFacebookUser t = do
  result <- liftIO $ simpleHttp $ concat [profileURL, "?access_token=", t]
  maybe (invalidArgs ["token2"]) (\fu -> do
    mu <- runDB $ getBy $ UniqueUserEmail $ email fu
    maybe (createUser fu) (\u -> updateUser u fu) mu) $ decode result
  where
    profileURL = "https://graph.facebook.com/me"
    createUser u = runDB $ insert $ User (name u) (email u) Nothing (gender u) (toSqlKey 1) (Just $ facebookId u) False Nothing (verified u) Nothing False Nothing
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
