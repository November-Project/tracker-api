module Handler.EmailSessions (postEmailSessionsR) where

import Import
import Helpers.Crypto
import Type.ErrorMessage
import Data.Aeson ((.:?))

postEmailSessionsR :: Handler Value
postEmailSessionsR = do
  s <- requireJsonBody :: Handler EmailAuth
  Entity uid u <- runDB $ getBy404 $ UniqueUserEmail $ email s
  maybe notFound (\b -> do
    case userUsesBcrypt u of
      False -> do
        if not $ validateMD5 b $ password s
          then sendResponseStatus status400 $ toJSON $ ErrorMessage "Invalid email or password."
          else do
            st <- liftIO $ getRandomToken 32
            _ <- runDB $ insert $ Session uid st (deviceInfo s) False
            newPassword <- liftIO $ encryptText $ password s
            runDB $ update uid
              [ UserUsesBcrypt =. True
              , UserPassword =. newPassword
              ]
            sendResponseStatus status201 $ object ["token" .= st]

      True -> do
        if not $ validateText b $ password s
          then sendResponseStatus status400 $ toJSON $ ErrorMessage "Invalid email or password."
          else do
            st <- liftIO $ getRandomToken 32
            _ <- runDB $ insert $ Session uid st (deviceInfo s) False
            sendResponseStatus status201 $ object ["token" .= st]
    ) $ userPassword u

data EmailAuth = EmailAuth { email :: Text, password :: Text, deviceInfo :: Maybe Text }

instance FromJSON EmailAuth where
  parseJSON (Object o) = EmailAuth
    <$> o .: "email"
    <*> o .: "password"
    <*> o .:? "device_info"

  parseJSON _ = mzero
