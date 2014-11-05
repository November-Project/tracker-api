module Handler.Session where

import Import
import Network.HTTP (urlDecode)
import Data.Text (pack)
import Helpers.Request

getSessionR :: String -> Handler Value
getSessionR token = do
  Entity _ s <- runDB $ getBy404 $ UniqueSessionToken $ pack $ urlDecode token
  if sessionExpired s
    then notAuthenticated
    else do
      u <- runDB $ get404 $ sessionUser s
      return $ object ["user" .= (Entity (sessionUser s) u)]

deleteSessionR :: String -> Handler ()
deleteSessionR token = do
  Entity sid s <- runDB $ getBy404 $ UniqueSessionToken $ pack $ urlDecode token
  requireUserSession $ sessionUser s
  runDB $ update sid [SessionExpired =. True]
  sendResponseStatus status200 ()
