module Handler.Events where

import Import hiding ((==.))
import Database.Esqueleto hiding (Value)
import Type.EventModel
import Helpers.Request

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  requireSession

  events <- runDB findEvents :: Handler [EventModel]
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
postEventsR tid = do
  requireTribeAdmin tid

  event <- requireJsonBody :: Handler Event
  _     <- runDB $ insert event
  sendResponseStatus status201 ("CREATED" :: Text)

