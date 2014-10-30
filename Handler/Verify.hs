module Handler.Verify where

import Import
import Network.HTTP (urlDecode)
import Data.Text (pack)

getVerifyR :: UserId -> String -> Handler ()
getVerifyR uid v = do
  user <- runDB $ get404 uid
  if userVerifyKey user == (Just $ pack $ urlDecode v)
    then do
      _ <- runDB $ update uid [UserVerifyKey =. Nothing]
      sendResponseStatus status200 ()
    else invalidArgs ["verify key"]
