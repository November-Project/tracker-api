module Handler.Event where

import Import
import qualified Database.Esqueleto as ES
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
      ES.select $
        ES.from $ \(event `ES.LeftOuterJoin` workout `ES.LeftOuterJoin` location) -> do
        ES.on $ event ES.^. EventLocation ES.==. location ES.?. LocationId
        ES.on $ event ES.^. EventWorkout ES.==. workout ES.?. WorkoutId
        ES.where_ (event ES.^. EventId ES.==. ES.val eid)
        return (event, workout, location)

putEventR :: TribeId -> EventId -> Handler Value
putEventR tid eid = do
  requireTribeAdmin tid

  e <- requireJsonBody :: Handler Event
  runDB $ replace eid e
  return $ object ["event" .= (Entity eid e)]

deleteEventR :: TribeId -> EventId -> Handler ()
deleteEventR tid eid = do
  requireTribeAdmin tid

  runDB $ deleteWhere [ResultEvent ==. eid]
  runDB $ delete eid
  sendResponseStatus status204 ()

