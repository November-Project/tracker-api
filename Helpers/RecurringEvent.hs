module Helpers.RecurringEvent where

import Import hiding (notElem, intersect)
import Helpers.Date
import Data.List (nubBy, notElem, foldl, intersect)

getRecurringEvents :: Day -> Day -> [Entity Event] -> [Event] -> [Event]
getRecurringEvents startDay endDay res es = nubBy dateCompare $ concat $ newEvents <$> prioritize res
  where
    weekCompare = negate . abs . eventWeek . entityVal
    prioritize = sortBy (\a b -> compare (weekCompare a) (weekCompare b))
    dateCompare e1 e2 = eventDate e1 == eventDate e2
    newEvents (Entity eid e) = (\date -> e
      { eventRecurringEvent = Just eid
      , eventDate = Just date
      , eventRecurring = False
      }) <$> newDates e
    newDates e = filter (`notElem` eventDates) $ recurringDates e
    recurringDates e = recurringDays (eventDays e) (eventWeek e) startDay endDay
    eventDates = catMaybes $ eventDate <$> es

doesDayClash :: Maybe Day -> Int -> [Int] -> Bool
doesDayClash day week daysOfWeek = case day of
  Nothing -> False
  Just d -> if weekOfMonth d /= week
    then False
    else dayOfWeek d `elem` daysOfWeek

eventScheduleOverlaps :: Event -> [Entity Event] -> Bool
eventScheduleOverlaps event events = foldl doDaysIntersect False $ map entityVal events
  where
    doDaysIntersect accum e = accum || (not $ null $ intersect (eventDays event) (eventDays e))

deleteEvent :: EventId -> Handler ()
deleteEvent eid = do
  runDB $ deleteWhere [ResultEvent ==. eid]
  runDB $ delete eid

deleteClashingEvents :: Event -> Handler ()
deleteClashingEvents event = case eventWeek event of
  0 -> return ()
  week -> do
    mevent <- runDB $ selectFirst [EventTribe ==. eventTribe event, EventRecurring ==. True, EventWeek ==. 0] []
    case mevent of
      Nothing -> return ()
      Just (Entity eid _) -> do
        allFutureEvents <- runDB $ selectList [EventRecurringEvent ==. Just eid] []
        let clashingEvents = filter (\e -> doesDayClash (eventDate $ entityVal e) week (eventDays event)) allFutureEvents
        sequence_ $ deleteEvent <$> map entityKey clashingEvents