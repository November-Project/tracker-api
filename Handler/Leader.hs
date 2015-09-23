module Handler.Leader where

import Import
import Helpers.Request

postLeaderR :: TribeId -> UserId -> Handler ()
postLeaderR tid uid = do
  requireTribeAdmin tid
  runDB $ update uid [UserTribeAdmin =. Just tid]
  sendResponseStatus status204 ()

deleteLeaderR :: TribeId -> UserId -> Handler ()
deleteLeaderR tid uid = do
  requireTribeAdmin tid
  runDB $ update uid [UserTribeAdmin =. Nothing]
  sendResponseStatus status204 ()

