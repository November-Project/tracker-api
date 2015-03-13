module Handler.Events where

import Import hiding ((==.), (<=.), (>=.), parseTime)
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.EventModel
import Data.Text (unpack)
import Data.Time (getCurrentTime, parseTime, UTCTime)
import System.Locale (defaultTimeLocale)

getEventsR :: TribeId -> Handler Value
getEventsR tid = do
  requireSession

  startTimeString <- lookupGetParam "start_date"
  endTimeString <- lookupGetParam "end_date"
  defaultTime <- lift getCurrentTime
  let startDate = maybe defaultTime id $ (unpack <$> startTimeString) >>= dateFromString
  let endDate = maybe defaultTime id $ (unpack <$> endTimeString) >>= dateFromString

  events <- runDB $ findEvents startDate endDate :: Handler [EventModel]
  return $ object ["events" .= events]
  where
    findEvents stime etime = do
      select $ 
        from $ \(event `LeftOuterJoin` workout `LeftOuterJoin` location) -> do
        on $ event ^. EventLocation ==. location ?. LocationId
        on $ event ^. EventWorkout ==. workout ?. WorkoutId
        where_ (event ^. EventTribe ==. val tid)
        where_ (event ^. EventDate >=. val stime)
        where_ (event ^. EventDate <=. val etime)
        let vc = sub_select $
                 from $ \v -> do
                 where_ (v ^. VerbalEvent ==. event ^. EventId)
                 return countRows
        let rc = sub_select $
                 from $ \r -> do
                 where_ (r ^. ResultEvent ==. event ^. EventId)
                 return countRows
        return (event, workout, location, vc, rc)

postEventsR :: TribeId -> Handler ()
postEventsR tid = do
  requireTribeAdmin tid
  event <- requireJsonBody :: Handler Event
  _     <- runDB $ insert event
  sendResponseStatus status201 ()

dateFromString :: String -> Maybe UTCTime
dateFromString = parseTime defaultTimeLocale "%Y-%m-%d"
