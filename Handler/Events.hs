module Handler.Events where

import Import hiding ((==.))
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.EventModel

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
        let vc = sub_select $
                 from $ \v -> do
                 where_ (v ^. VerbalEvent ==. event ^. EventId)
                 return countRows
        let rc = sub_select $
                 from $ \r -> do
                 where_ (r ^. ResultEvent ==. event ^. EventId)
                 return countRows
        return (event, workout, location, vc, rc)

postEventsR :: TribeId -> Handler ()
postEventsR tid = do
  requireTribeAdmin tid
  event <- requireJsonBody :: Handler Event
  _     <- runDB $ insert event
  sendResponseStatus status201 ()

