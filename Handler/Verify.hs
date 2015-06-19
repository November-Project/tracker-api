module Handler.Verify where

import Import hiding (urlDecode)
import Network.HTTP (urlDecode)

getVerifyR :: UserId -> String -> Handler ()
getVerifyR uid v = do
  user <- runDB $ get404 uid
  if userVerifyKey user == (Just $ pack $ urlDecode v)
    then do
      _ <- runDB $ update uid [UserVerifyKey =. Nothing, UserIsVerified =. True]
      sendResponseStatus status200 ()
    else invalidArgs ["verify key"]
