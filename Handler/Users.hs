module Handler.Users where

import Import hiding (concat)
import Helpers.Crypto
import Helpers.Request
import Data.Text (concat)

getUsersR :: Handler Value
getUsersR = do
  requireAnyAdmin

  term <- lookupGetParam "q"
  tribe <- lookupGetParam "tribe"
  let filters = case (term, fromPathPiece =<< tribe) of
                  (Just searchTerm, Just tribeID) -> Just $ searchFilters searchTerm ++ tribeFilter tribeID
                  (Just searchTerm, Nothing) -> Just $ searchFilters searchTerm
                  (Nothing, Just tribeID) -> Just $ tribeFilter tribeID
                  (_, _) -> Nothing

  users <- maybe (pure []) userSearch filters
  return $ object ["users" .= users]
  where
    ilike field val = Filter field (Left $ concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")
    userSearch filters = runDB $ selectList filters [] :: Handler [Entity User]
    searchFilters t = [UserEmail `ilike` t] ||. [UserName `ilike` t]
    tribeFilter t = [UserTribe ==. t]

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
