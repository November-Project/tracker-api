module Handler.Recurring where

import Import
import Data.List (foldl)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.Date
import Type.RecurringModel
import Type.ErrorMessage

getRecurringR :: TribeId -> RecurringId -> Handler Value
getRecurringR _ rid = do
  requireSession
  
  recurring <- runDB findRecurring :: Handler [RecurringModel]

  case recurring of
    [] -> sendResponseStatus status404 ()
    (r:_) -> return $ object ["recurring" .= r]
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
  rs <- runDB $ selectList [RecurringTribe ==. tid] []
  
  let doesConflict = foldl (\a x -> a || doesScheduleConflict (schedule r) (schedule x)) False $ map entityVal rs
  
  if doesConflict
    then return $ toJSON $ ErrorMessage "Schedule conflicts with other recurring events."
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

