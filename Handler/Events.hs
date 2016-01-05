module Handler.Events where

import Import
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.Date
import Helpers.RecurringEvent
import Type.EventModel
import Type.ErrorMessage

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  requireSession

  startTimeString <- lookupGetParam "start_date"
  endTimeString <- lookupGetParam "end_date"
  now <- utctDay <$> liftIO getCurrentTime

  let startDate = fromMaybe now $ unpack <$> startTimeString >>= parseGregorianDate
  let endDate = fromMaybe now $ unpack <$> endTimeString >>= parseGregorianDate

  allEventEntities <- runDB $ selectList (
    [ EventDate >=. Just startDate
    , EventDate <=. Just endDate
    ] ||.
    [ EventRecurring ==. True
    ]) [] :: Handler [Entity Event]

  let allEvents = (\(Entity _ e) -> e) <$> allEventEntities 
  let recurringEvents = filter (\(Entity _ e) -> eventRecurring e) allEventEntities

  let newEvents = getRecurringEvents startDate endDate recurringEvents allEvents
  _ <- sequence_ $ runDB . insert <$> newEvents

  events <- runDB $ findEvents startDate endDate :: Handler [EventModel]
  return $ object ["events" .= events]
  where
    findEvents stime etime = do
      ES.select $
        ES.from $ \(event `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ event ES.^. EventLocation ES.==. location ES.?. LocationId
        ES.on $ event ES.^. EventWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ (event ES.^. EventTribe ES.==. ES.val tid)
        ES.where_ (event ES.^. EventDate ES.>=. ES.val (Just stime))
        ES.where_ (event ES.^. EventDate ES.<=. ES.val (Just etime))
        return (event, workout, location)

postEventsR :: TribeId -> Handler Value
postEventsR tid = do
  requireTribeAdmin tid
  event <- requireJsonBody :: Handler Event

  if eventRecurring event
    then postRecurringEvent event
    else do
      eid <- runDB $ insert event
      sendResponseStatus status201 $ object ["event" .= Entity eid event]

postRecurringEvent :: Event -> Handler Value
postRecurringEvent event = do
  sameWeekEvents <- runDB $ selectList [EventTribe ==. eventTribe event, EventRecurring ==. True, EventWeek ==. eventWeek event] []

  if eventScheduleOverlaps event sameWeekEvents
    then sendResponseStatus status400 $ toJSON $ ErrorMessage "Recurring Event with this schedule already exists."
    else do
      deleteClashingEvents event
      
      -- Recurring events shouldn't have dates
      eid <- runDB $ insert event { eventDate = Nothing }
      sendResponseStatus status201 $ object ["event" .= Entity eid event]
