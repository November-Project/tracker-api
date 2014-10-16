module Handler.Event where 

import Import hiding ((==.))
import Database.Esqueleto hiding (Value)
import Type.EventModel
import Type.EventFullModel
import Type.VerbalUser
import Type.ResultUser

getEventR :: TribeId -> EventId -> Handler Value
getEventR _ eid = do
  event <- runDB findEvent :: Handler [EventModel]
  case event of
    [] -> do sendResponseStatus status404 ("NOT FOUND" :: Text)
    (e:_) -> do
      v <- runDB getEventVerbals :: Handler [VerbalUser]
      r <- runDB getEventResults :: Handler [ResultUser]
      return $ object ["event" .= ((e, v, r) :: EventFullModel)]
  where
    findEvent =
      select $
        from $ \(event `LeftOuterJoin` workout `LeftOuterJoin` location) -> do
        on $ event ^. EventLocation ==. location ?. LocationId
        on $ event ^. EventWorkout ==. workout ?. WorkoutId
        where_ (event ^. EventId ==. val eid)
        return (event, workout, location)
    getEventVerbals =
      select $  
        from $ \(v `InnerJoin` u) -> do 
        on $ v ^. VerbalUser ==. u ^. UserId
        where_ (v ^. VerbalEvent ==. val eid) 
        return (v, u)
    getEventResults =
      select $  
        from $ \(r `InnerJoin` u) -> do 
        on $ r ^. ResultUser ==. u ^. UserId
        where_ (r ^. ResultEvent ==. val eid) 
        return (r, u)

putEventR :: TribeId -> EventId -> Handler ()
putEventR _ eid = do
  event <- requireJsonBody :: Handler Event
  runDB $ replace eid event
  sendResponseStatus status200 ("UPDATED" :: Text)

