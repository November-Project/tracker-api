module Handler.Events where

import Import hiding (notElem, minimum)
import qualified Database.Esqueleto as ES
import Data.Time.LocalTime
import Helpers.Request
import Helpers.Date
import Helpers.TimeZone
import Type.EventModel
import Data.List (nubBy, notElem, minimum)

createRecurringEvents :: Day -> Day -> [Recurring] -> [Event] -> [Event]
createRecurringEvents startDay endDay res es = nubBy unique $ concat $ newEvents <$> res
  where
    unique e1 e2 = eventDate e1 == eventDate e2
    newEvents r@Recurring{..} = (\date -> Event
      recurringTitle recurringTribe date recurringTimes recurringHideWorkout recurringLocation recurringWorkout
      ) <$> newDates r
    newDates r = filter (`notElem` eventDates) $ recurringDates r
    recurringDates Recurring{..} = getRecurringDays recurringDays recurringWeek startDay endDay
    eventDates = eventDate <$> es

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  requireSession

  startTimeString <- lookupGetParam "start_date"
  endTimeString <- lookupGetParam "end_date"
  now <- utctDay <$> liftIO getCurrentTime

  let startDate = fromMaybe now $ unpack <$> startTimeString >>= parseGregorianDate
  let endDate = fromMaybe now $ unpack <$> endTimeString >>= parseGregorianDate

  -- Create any recurring events in the past between these dates
  localDate <- localDay <$> localTimeForTribe tid
  _ <- if localDate > startDate
    then do
      es <- runDB $ selectList [EventTribe ==. tid, EventDate >=. startDate, EventDate <=. endDate] []
      recurrings <- runDB $ selectList [RecurringTribe ==. tid] []
      let newEvents = createRecurringEvents startDate (minimum [endDate, localDate]) (entityVal <$> recurrings) (entityVal <$> es)
      _ <- sequence_ $ runDB . insert <$> newEvents
      return ()
    else return ()

  events <- runDB $ findEvents startDate endDate :: Handler [EventModel]
  return $ object ["events" .= events]
  where
    findEvents stime etime = do
      ES.select $
        ES.from $ \(event `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ event ES.^. EventLocation ES.==. location ES.?. LocationId
        ES.on $ event ES.^. EventWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ $ event ES.^. EventTribe ES.==. ES.val tid
        ES.where_ $ event ES.^. EventDate ES.>=. ES.val stime
        ES.where_ $ event ES.^. EventDate ES.<=. ES.val etime
        return (event, workout, location)

postEventsR :: TribeId -> Handler Value
postEventsR tid = do
  requireTribeAdmin tid

  event <- requireJsonBody :: Handler Event
  eid <- runDB $ insert event
  sendResponseStatus status201 $ object ["event" .= Entity eid event]

