module Handler.Result where

import Import

getResultR :: TribeId -> EventId -> ResultId -> Handler Value
getResultR _ _ rid = do
  result <- runDB $ get404 rid
  return $ object ["result" .= Entity rid result]

putResultR :: TribeId -> EventId -> ResultId -> Handler ()
putResultR _ _ rid = do
  result <- requireJsonBody :: Handler Result
  runDB $ replace rid result
  sendResponseStatus status200 ("UPDATED" :: Text)

