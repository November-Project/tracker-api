module Handler.Events where

import Import hiding ((==.))
import Database.Esqueleto hiding (Value)

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  events <- runDB findEvents
  return $ object ["events" .= events]
  where
    findEvents =
      select $ 
        from $ \(event `LeftOuterJoin` workout `LeftOuterJoin` location) -> do
        on $ event ^. EventLocation ==. location ?. LocationId
        on $ event ^. EventWorkout ==. workout ?. WorkoutId
        where_ (event ^. EventTribe ==. val tid)
        return (event, workout, location)

postEventsR :: TribeId -> Handler ()
postEventsR _ = do
  event <- requireJsonBody :: Handler Event
  _     <- runDB $ insert event
  sendResponseStatus status201 ("CREATED" :: Text)

