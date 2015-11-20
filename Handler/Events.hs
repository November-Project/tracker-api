module Handler.Events where

import Import hiding (notElem)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.Date
import Type.EventModel
import Data.List (nubBy, notElem)

createRecurringEvents :: Day -> Day -> [Entity Event] -> [Event] -> [Event]
createRecurringEvents startDay endDay res es = nubBy unique $ concat $ newEvents <$> res
  where
    unique e1 e2 = eventDate e1 == eventDate e2
    newEvents (Entity eid e) = (\date -> e
      { eventRecurringEvent = Just eid
      , eventDate = date
      , eventRecurring = False
      }) <$> newDates e
    newDates e = filter (`notElem` eventDates) $ recurringDates e
    recurringDates e = recurringDays (eventDays e) (eventWeek e) startDay endDay
    eventDates = eventDate <$> es

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  requireSession

  startTimeString <- lookupGetParam "start_date"
  endTimeString <- lookupGetParam "end_date"
  now <- utctDay <$> liftIO getCurrentTime

  let startDate = fromMaybe now $ unpack <$> startTimeString >>= parseGregorianDate
  let endDate = fromMaybe now $ unpack <$> endTimeString >>= parseGregorianDate

  allEventEntities <- runDB $ selectList (
    [ EventDate >=. startDate
    , EventDate <=. endDate
    ] ||.
    [ EventRecurring ==. True
    ]) [] :: Handler [Entity Event]

  let allEvents = (\(Entity _ e) -> e) <$> allEventEntities 
  let recurringEvents = filter (\(Entity _ e) -> eventRecurring e) allEventEntities

  let newEvents = createRecurringEvents startDate endDate recurringEvents allEvents
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
        ES.where_ $ ((event ES.^. EventDate ES.>=. ES.val stime)
          ES.&&. (event ES.^. EventDate ES.<=. ES.val etime))
          ES.||. (event ES.^. EventRecurring ES.==. ES.val True)
        return (event, workout, location)

postEventsR :: TribeId -> Handler Value
postEventsR tid = do
  requireTribeAdmin tid
  event <- requireJsonBody :: Handler Event
  eid   <- runDB $ insert event
  sendResponseStatus status201 $ object ["event" .= Entity eid event]

