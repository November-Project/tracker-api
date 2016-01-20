module Handler.Recurrings where

import Import
import Data.List (foldl)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.RecurringEvent
import Type.RecurringModel
import Type.ErrorMessage

getRecurringsR :: TribeId -> Handler Value
getRecurringsR tid = do
  requireSession

  recurrings <- runDB findRecurrings :: Handler [RecurringModel]
  return $ object ["recurrings" .= recurrings]
  where
    findRecurrings = 
      ES.select $
        ES.from $ \(recurring `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ recurring ES.^. RecurringLocation ES.==. location ES.?. LocationId
        ES.on $ recurring ES.^. RecurringWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ $ recurring ES.^. RecurringTribe ES.==. ES.val tid
        return (recurring, workout, location)

postRecurringsR :: TribeId -> Handler Value
postRecurringsR tid = do
  requireTribeAdmin tid

  r <- requireJsonBody :: Handler Recurring
  rs <- runDB $ selectList [RecurringTribe ==. tid] []
  
  let doesConflict = foldl (\a x -> a || doesScheduleConflict (schedule r) (schedule x)) False $ map entityVal rs
  
  if doesConflict
    then sendResponseStatus status400 $ toJSON $ ErrorMessage "Schedule conflicts with other recurring events."
    else do
      rid <- runDB $ insert r
      sendResponseStatus status201 $ object ["recurring" .= Entity rid r]
  where
    schedule r = (recurringWeek r, recurringDays r)
