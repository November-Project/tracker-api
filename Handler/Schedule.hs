module Handler.Schedule where

import Import

getScheduleR :: TribeId -> ScheduleId -> Handler Value
getScheduleR _ sid = do
  schedule <- runDB $ get404 sid
  return $ object ["schedule" .= Entity sid schedule]

putScheduleR :: TribeId -> ScheduleId -> Handler ()
putScheduleR _ sid = do
  schedule <- requireJsonBody :: Handler Schedule
  runDB $ replace sid schedule
  sendResponseStatus status200 ("UPDATED" :: Text)
