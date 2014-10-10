module Handler.Event where

import Import

getEventR :: TribeId -> EventId -> Handler Value
getEventR _ eid = do
  event <- runDB $ get404 eid
  return $ object ["event" .= Entity eid event]

putEventR :: TribeId -> EventId -> Handler ()
putEventR _ eid = do
  event <- requireJsonBody :: Handler Event
  runDB $ replace eid event
  sendResponseStatus status200 ("UPDATED" :: Text)
