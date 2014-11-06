module Handler.Location where

import Import
import Helpers.Request

getLocationR :: TribeId -> LocationId -> Handler Value
getLocationR _ lid = do
  requireSession
  l <- runDB $ get404 lid
  return $ object ["location" .= Entity lid l]

putLocationR :: TribeId -> LocationId -> Handler ()
putLocationR tid lid = do
  requireTribeAdmin tid
  l <- requireJsonBody :: Handler Location
  runDB $ replace lid l
  sendResponseStatus status200 ()

