module Handler.Results where

import Import

postResultsR :: TribeId -> EventId -> Handler ()
postResultsR _ _ = do
  result <- requireJsonBody :: Handler Result
  _      <- runDB $ insert result
  sendResponseStatus status201 ("CREATED" :: Text)

