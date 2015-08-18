module Handler.Locations where

import Import
import Helpers.Request

getLocationsR :: TribeId -> Handler Value
getLocationsR tid = do
  requireSession
  locations <- runDB $ selectList [LocationTribe ==. tid] [] :: Handler [Entity Location]
  return $ object ["locations" .= locations]

postLocationsR :: TribeId -> Handler Value
postLocationsR tid = do
  requireTribeAdmin tid
  location <- requireJsonBody :: Handler Location
  lid      <- runDB $ insert location
  sendResponseStatus status201 $ object ["location" .= Entity lid location]

