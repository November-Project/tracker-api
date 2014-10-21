module Handler.Home where

import Import
import Helpers.Request

getHomeR :: TribeId -> Handler Value
getHomeR tid = do
  requireTribeAdmin tid
  sendResponseStatus status200 ("SICK DUDE" :: Text)
