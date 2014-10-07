module Handler.Locations where

import Import

getLocationsR :: Handler Value
getLocationsR = do
  locations <- runDB $ selectList [] [] :: Handler [Entity Location]
  return $ object ["locations" .= locations]

