module Handler.EmailSessions (postEmailSessionsR) where

import Import
import Helpers.Crypto
import Control.Monad (mzero)
import Data.Aeson ((.:?))

postEmailSessionsR :: Handler Value
postEmailSessionsR = do
  s <- requireJsonBody :: Handler EmailAuth
  Entity uid u <- runDB $ getBy404 $ UniqueUserEmail $ email s
  maybe notFound (\b -> do
    if not $ validateText b $ password s
      then invalidArgs ["email", "password"]
      else do
        st <- liftIO $ getRandomToken 32
        _ <- runDB $ insert $ Session uid st $ deviceInfo s
        sendResponseStatus status200 st) $ userPassword u

data EmailAuth = EmailAuth { email :: Text, password :: Text, deviceInfo :: Maybe Text }

instance FromJSON EmailAuth where
  parseJSON (Object o) = EmailAuth
    <$> o .: "email"
    <*> o .: "password"
    <*> o .:? "device_info"

  parseJSON _ = mzero
