module Handler.Verbal where

import Import

deleteVerbalR :: TribeId -> EventId -> VerbalId -> Handler ()
deleteVerbalR _ _ vid = do
  runDB $ delete vid
  sendResponseStatus status200 ("DELETED" :: Text)
