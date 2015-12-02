module Handler.Event where

import Import
import qualified Database.Esqueleto as ES
import Helpers.Request
import Type.EventModel

getEventR :: TribeId -> EventId -> Handler Value
getEventR _ eid = do
  requireSession
  event <- runDB findEvent :: Handler [EventModel]
  case event of
    [] -> sendResponseStatus status404 ()
    (e:_) -> return $ object ["event" .= e]
  where
    findEvent =
      ES.select $
        ES.from $ \(event `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ event ES.^. EventLocation ES.==. location ES.?. LocationId
        ES.on $ event ES.^. EventWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ (event ES.^. EventId ES.==. ES.val eid)
        return (event, workout, location)

putEventR :: TribeId -> EventId -> Handler Value
putEventR tid eid = do
  requireTribeAdmin tid
  e <- requireJsonBody :: Handler Event

  -- if not recurring clear out eventRecurringEvent and update as normal
  if eventRecurring e
    then do
      -- save recurring event changes and make sure there isn't a date
      runDB $ replace eid e { eventDate = Nothing }
      now <- utctDay <$> liftIO getCurrentTime
      runDB $ updateWhere
        [
          EventRecurringEvent ==. Just eid,
          EventDate >. Just now
        ]
        [
          EventTimes =. eventTimes e,
          EventHideWorkout =. eventHideWorkout e,
          EventLocation =. eventLocation e,
          EventWorkout =. eventWorkout e
        ]
    else
      -- Save edited event and make sure it's not linked to a recurring event
      runDB $ replace eid e { eventRecurringEvent = Nothing }

  return $ object ["event" .= (Entity eid e)]

deleteEventR :: TribeId -> EventId -> Handler ()
deleteEventR tid eid = do
  requireTribeAdmin tid

  e <- runDB $ get404 eid

  if eventRecurring e
    then do
      now <- utctDay <$> liftIO getCurrentTime
      -- Update past events to remove recurring event reference
      runDB $ updateWhere
        [EventRecurringEvent ==. Just eid, EventDate <=. Just now]
        [EventRecurringEvent =. Nothing]
      -- Delete all future events
      events <- runDB $ selectList [EventRecurringEvent ==. Just eid, EventDate >. Just now] []
      mapM_ deleteEventEntity events
      -- Delete actual event
      deleteEvent eid
    else
      deleteEvent eid

  sendResponseStatus status204 ()

  where
    deleteEventEntity (Entity i _) = deleteEvent i
    deleteEvent i = do
      runDB $ deleteWhere [VerbalEvent ==. i]
      runDB $ deleteWhere [ResultEvent ==. i]
      runDB $ delete i
      
