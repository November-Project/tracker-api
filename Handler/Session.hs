module Handler.Session where

import Import
import Helpers.Request

deleteSessionR :: Handler ()
deleteSessionR = do
  Entity sid s <- getSessionFromHeader
  requireUserSession $ sessionUser s
  runDB $ update sid [SessionExpired =. True]
  sendResponseStatus status200 ()
