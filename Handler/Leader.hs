module Handler.Leader where

import Import
import Helpers.Request

putLeaderR :: TribeId -> UserId -> Handler ()
putLeaderR tid uid = do
  requireAdmin
  runDB $ update uid [UserTribeAdmin =. Just tid]
  sendResponseStatus status200 ()

deleteLeaderR :: TribeId -> UserId -> Handler ()
deleteLeaderR _ uid = do
  requireAdmin
  runDB $ update uid [UserTribeAdmin =. Nothing]
  sendResponseStatus status200 ()
