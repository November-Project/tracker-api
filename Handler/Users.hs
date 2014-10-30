module Handler.Users where

import Import
import Helpers.Crypto

postUsersR :: Handler ()
postUsersR = do
  user <- requireJsonBody :: Handler User
  case (userPassword user, userFacebookId user) of
    (Just p, Nothing) -> do
      v <- liftIO $ getRandomToken 32
      e <- liftIO $ encryptText p
      insertUser $ user { userPassword = e, userVerifyKey = Just v }
    (Nothing, Just _) -> insertUser user
    _ -> invalidArgs ["password"]

insertUser :: User -> Handler ()
insertUser u = do
  _ <- runDB $ insert u
  sendResponseStatus status201 ()
