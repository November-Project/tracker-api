module Handler.Verbals where

import Import

getVerbalsR :: TribeId -> EventId -> Handler Value
getVerbalsR _ eid = do
  verbals <- runDB $ selectList [VerbalEvent ==. eid] [] :: Handler [Entity Verbal]
  return $ object ["verbals" .= verbals]

postVerbalsR :: TribeId -> EventId -> Handler ()
postVerbalsR _ _ = do
  verbal <- requireJsonBody :: Handler Verbal
  _      <- runDB $ insert verbal
  sendResponseStatus status201 ("CREATED" :: Text)
