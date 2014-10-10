module Handler.Locations where

import Import

getLocationsR :: TribeId -> Handler Value
getLocationsR tid = do
  locations <- runDB $ selectList [LocationTribe ==. tid] [] :: Handler [Entity Location]
  return $ object ["locations" .= locations]

postLocationsR :: TribeId -> Handler ()
postLocationsR _ = do
  location <- requireJsonBody :: Handler Location
  _        <- runDB $ insert location

  sendResponseStatus status201 ("CREATED" :: Text)

