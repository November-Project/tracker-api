module Helpers.Facebook (createOrUpdateFacebookUser) where

import Import
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode)
import Database.Persist.Sql (toSqlKey)

createOrUpdateFacebookUser :: String -> Handler UserId
createOrUpdateFacebookUser t = do
  result <- liftIO $ simpleHttp $ concat [ profileURL
                                         , "?access_token="
                                         , t
                                         , "&fields=email,name,gender,verified"
                                         ]

  maybe (invalidArgs ["token2"]) (\fu -> do
    muEmail <- runDB $ getBy $ UniqueUserEmail $ email fu
    muFacebook <- runDB $ getBy $ UniqueUserFacebookId $ Just $ facebookId fu
    case (muEmail, muFacebook) of
      (Just uEmail, _) -> updateUser uEmail fu
      (_, Just (Entity uid _)) -> return uid
      (_, _) -> createUser fu
    ) $ decode result
  where
    profileURL = "https://graph.facebook.com/v2.7/me"
    createUser u = runDB $ insert $ User (name u) (email u) Nothing (gender u) (toSqlKey 1) (Just $ "http://graph.facebook.com/v2.7/" ++ facebookId u  ++ "/picture") (Just $ facebookId u) False Nothing (verified u) Nothing False Nothing True
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
