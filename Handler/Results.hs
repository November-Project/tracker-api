module Handler.Results where

import Import
import Helpers.Request

postResultsR :: TribeId -> EventId -> Handler ()
postResultsR _ _ = do
  r <- requireJsonBody :: Handler Result
  requireUserSession $ resultUser r
  _      <- runDB $ insert r
  sendResponseStatus status201 ()

