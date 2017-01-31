module Handler.ProcessEvents where

import Import hiding (head, notElem)
import Data.List (head, notElem)
import Data.Time.LocalTime
import Helpers.Date
import Helpers.TimeZone

postProcessEventsR :: Handler ()
postProcessEventsR = do
  tribes <- runDB $ selectList [] [] :: Handler [Entity Tribe]
  _ <- sequence_ $ map processRecurring tribes
  sendResponseStatus status200 Null

processRecurring :: Entity Tribe -> Handler ()
processRecurring (Entity tid t) = do
  now <- localTimeForTribe tid
  let nowDay = localDay now

  -- If not a valid day, do nothing
  if notElem (dayOfWeek nowDay) (tribeDaysOfWeek t)
    then return ()
    else do
      event <- runDB $ getBy $ UniqueEventTribeDate tid nowDay

      -- If day is already taken, do nothing
      case event of
        Just _ -> return ()
        Nothing -> do
          rs <- runDB $ selectList [RecurringTribe ==. tid] []
          let recurring = listToMaybe $ sortBy sorter $ filter (fitsSchedule nowDay) $ entityVal <$> rs

          -- If no recurring event fits the day, do nothing
          case recurring of
            Nothing -> return ()
            Just r@Recurring{..} -> do
              let time = head $ sort recurringTimes
              let eventTime = now { localTimeOfDay = time }

              -- If event in the future, do nothing
              if now > eventTime
                then void $ runDB $ insert $ createEvent nowDay r
                else return ()
  where
    fitsSchedule nowDay Recurring{..} =
      dayOfWeek nowDay `elem` recurringDays &&
      recurringWeek `elem` [0, weekOfMonth nowDay, weekOfMonth' nowDay]
    sorter lhs rhs
      | recurringWeek lhs == recurringWeek rhs = EQ
      | recurringWeek rhs == 0 = LT
      | recurringWeek lhs == 0 = GT
      | otherwise = compare (recurringWeek rhs) (recurringWeek lhs)
    createEvent nowDay Recurring{..} = Event
      recurringTitle recurringTribe nowDay recurringTimes
      recurringHideWorkout recurringLocation recurringWorkout recurringTags

