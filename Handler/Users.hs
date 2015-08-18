module Handler.Users where

import Import hiding (concat)
import Helpers.Crypto
import Helpers.Request
import Data.Text (concat)

getUsersR :: Handler Value
getUsersR = do
  requireAdmin
  
  term <- lookupGetParam "q"
  let ilike field val = Filter field (Left $ concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")

  case term of
    Just t -> do
      users <- runDB $ selectList ([UserEmail `ilike` t] ||. [UserName `ilike` t]) [] :: Handler [Entity User]
      return $ object ["users" .= users]
    Nothing -> return $ object ["users" .= ()]

postUsersR :: Handler Value
postUsersR = do
  user <- requireJsonBody :: Handler User
  case (userPassword user, userFacebookId user) of
    (Just p, Nothing) -> do
      v <- liftIO $ getRandomToken 32
      e <- liftIO $ encryptText p
      insertUser $ user { userPassword = e, userVerifyKey = Just v }
    (Nothing, Just _) -> insertUser user
    _ -> invalidArgs ["password"]

insertUser :: User -> Handler Value
insertUser u = do
  uid <- runDB $ insert u
  sendResponseStatus status201 $ object ["user" .= Entity uid u]
