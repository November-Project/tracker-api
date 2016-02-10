module Handler.Events where

import Import
import qualified Database.Esqueleto as ES
import Helpers.Request
import Helpers.Date
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
  if eventTimes event == []
    then sendResponseStatus status400 $ toJSON $ ErrorMessage "You must pick a time for an event."
    else do
      eid <- runDB $ insert event
      sendResponseStatus status201 $ object ["event" .= Entity eid event]

