module Handler.Locations where

import Import
import Helpers.Request

getLocationsR :: TribeId -> Handler Value
getLocationsR tid = do
  requireSession

  locations <- runDB $ selectList [LocationTribe ==. tid] [] :: Handler [Entity Location]
  return $ object ["locations" .= locations]

postLocationsR :: TribeId -> Handler ()
postLocationsR tid = do
  requireTribeAdmin tid

  location <- requireJsonBody :: Handler Location
  _        <- runDB $ insert location

  sendResponseStatus status201 ("CREATED" :: Text)

