module Handler.Verbals where

import Import

postVerbalsR :: TribeId -> EventId -> Handler ()
postVerbalsR _ _ = do
  verbal <- requireJsonBody :: Handler Verbal
  _      <- runDB $ insert verbal
  sendResponseStatus status201 ("CREATED" :: Text)
