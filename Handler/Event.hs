module Handler.Event where

import Import hiding ((==.), on)
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.EventModel

getEventR :: TribeId -> EventId -> Handler Value
getEventR _ eid = do
  requireSession
  event <- runDB findEvent :: Handler [EventModel]
  case event of
    [] -> sendResponseStatus status404 ()
    (e:_) -> return $ object ["event" .= e]
  where
    findEvent =
      select $
        from $ \(event `LeftOuterJoin` workout `LeftOuterJoin` location) -> do
        on $ event ^. EventLocation ==. location ?. LocationId
        on $ event ^. EventWorkout ==. workout ?. WorkoutId
        where_ (event ^. EventId ==. val eid)
        let vc = sub_select $
                 from $ \v -> do
                 where_ (v ^. VerbalEvent ==. event ^. EventId)
                 return countRows
        let rc = sub_select $
                 from $ \r -> do
                 where_ (r ^. ResultEvent ==. event ^. EventId)
                 return countRows
        return (event, workout, location, vc, rc)

putEventR :: TribeId -> EventId -> Handler ()
putEventR tid eid = do
  requireTribeAdmin tid
  event <- requireJsonBody :: Handler Event
  runDB $ replace eid event
  sendResponseStatus status200 ()

