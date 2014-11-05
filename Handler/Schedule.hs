module Handler.Schedule where

import Import
import Helpers.Request

getScheduleR :: TribeId -> ScheduleId -> Handler Value
getScheduleR _ sid = do
  requireSession
  schedule <- runDB $ get404 sid
  return $ object ["schedule" .= Entity sid schedule]

putScheduleR :: TribeId -> ScheduleId -> Handler ()
putScheduleR tid sid = do
  requireTribeAdmin tid
  s <- requireJsonBody :: Handler Schedule
  runDB $ update sid [ScheduleTime =. scheduleTime s, ScheduleLocation =. scheduleLocation s]
  sendResponseStatus status200 ()

deleteScheduleR :: TribeId -> ScheduleId -> Handler ()
deleteScheduleR _ sid = do
  requireAdmin
  runDB $ delete sid
  sendResponseStatus status200 ()
