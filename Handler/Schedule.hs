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

  schedule <- requireJsonBody :: Handler Schedule
  runDB $ replace sid schedule
  sendResponseStatus status200 ("UPDATED" :: Text)
