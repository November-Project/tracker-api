module Handler.Results where

import Import
import Helpers.Request

postResultsR :: TribeId -> EventId -> Handler ()
postResultsR _ _ = do
  result <- requireJsonBody :: Handler Result
  requireUserSession $ resultUser result
  _      <- runDB $ insert result
  sendResponseStatus status201 ("CREATED" :: Text)

