module Handler.Reset where

import Import
import Helpers.Crypto

postResetR :: Handler ()
postResetR = do
  r <- requireJsonBody :: Handler ResetData
  mu <- runDB $ selectFirst [UserForgotToken ==. Just (token r)] []
  case mu of
    Nothing -> notFound
    Just (Entity uid _) -> do
      p <- liftIO $ encryptText $ password r
      runDB $ update uid
        [ UserForgotToken =. Nothing
        , UserPassword =. p
        ]
      sendResponseStatus status201 ()

data ResetData = ResetData
  { password :: Text
  , token :: Text
  }

instance FromJSON ResetData where
  parseJSON (Object o) = ResetData
    <$> o .: "password"
    <*> o .: "token"

  parseJSON _ = mzero
