module Handler.Verbal where

import Import
import Helpers.Request

deleteVerbalR :: TribeId -> VerbalId -> Handler ()
deleteVerbalR _ vid = do
  v <- runDB $ get404 vid
  requireUserSession $ verbalUser v
  runDB $ delete vid
  sendResponseStatus status204 ()
