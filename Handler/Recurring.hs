module Handler.Recurring where

import Import
import Data.List (foldl)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.RecurringEvent
import Type.RecurringModel
import Type.ErrorMessage

getRecurringR :: TribeId -> RecurringId -> Handler Value
getRecurringR _ rid = do
  requireSession

  recurring <- runDB findRecurring :: Handler [RecurringTuple]

  case recurring of
    [] -> sendResponseStatus status404 ()
    ((r, w, l):_) -> return $ object ["recurring" .= RecurringModel r w l]
  where
    findRecurring =
      ES.select $
        ES.from $ \(recurring `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ recurring ES.^. RecurringLocation ES.==. location ES.?. LocationId
        ES.on $ recurring ES.^. RecurringWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ $ recurring ES.^. RecurringId ES.==. ES.val rid
        return (recurring, workout, location)

putRecurringR :: TribeId -> RecurringId -> Handler Value
putRecurringR tid rid = do
  requireTribeAdmin tid

  r <- requireJsonBody :: Handler Recurring
  if recurringTimes r == []
    then sendResponseStatus status400 $ toJSON $ ErrorMessage "You must pick a time for an event."
    else do
      rs <- runDB $ selectList [RecurringTribe ==. tid] []
      oldr <- runDB $ get404 rid

      let isDifferent = schedule r /= schedule oldr
      let doesConflict = (isDifferent &&) $ foldl (\a x -> a || doesScheduleConflict (schedule r) (schedule x)) False $ map entityVal rs

      if doesConflict
        then sendResponseStatus status400 $ toJSON $ ErrorMessage "Schedule conflicts with other recurring events."
        else do
          runDB $ replace rid r
          return $ object ["recurring" .= (Entity rid r)]
  where
    schedule r = (recurringWeek r, recurringDays r)

deleteRecurringR :: TribeId -> RecurringId -> Handler ()
deleteRecurringR tid rid = do
  requireTribeAdmin tid

  runDB $ delete rid
  sendResponseStatus status204 ()

