module Handler.Result where

import Import
import Helpers.Request

putResultR :: TribeId -> EventId -> ResultId -> Handler ()
putResultR _ _ rid = do
  result <- requireJsonBody :: Handler Result
  requireUserSession $ resultUser result
  runDB $ replace rid result
  sendResponseStatus status200 ("UPDATED" :: Text)

