module Handler.Locations where

import Import

getLocationsR :: Handler Value
getLocationsR = do
  locations <- runDB $ selectList [] [] :: Handler [Entity Location]
  return $ object ["locations" .= locations]

postLocationsR :: Handler ()
postLocationsR = do
  location <- requireJsonBody :: Handler Location
  _        <- runDB $ insert location

  sendResponseStatus status201 ("CREATED" :: Text)

