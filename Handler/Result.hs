module Handler.Result where

import Import

putResultR :: TribeId -> EventId -> ResultId -> Handler ()
putResultR _ _ rid = do
  result <- requireJsonBody :: Handler Result
  runDB $ replace rid result
  sendResponseStatus status200 ("UPDATED" :: Text)

