module Handler.Verbals where

import Import
import Helpers.Request

postVerbalsR :: TribeId -> EventId -> Handler ()
postVerbalsR _ _ = do
  verbal <- requireJsonBody :: Handler Verbal
  requireUserSession $ verbalUser verbal
  _ <- runDB $ insert verbal
  sendResponseStatus status201 ()
