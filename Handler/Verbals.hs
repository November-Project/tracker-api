module Handler.Verbals where

import Import hiding ((==.), on)
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.VerbalUser

getVerbalsR :: TribeId -> EventId -> Handler Value
getVerbalsR _ eid = do
  requireSession
  vs <- runDB selectVerbals :: Handler [VerbalUser]
  return $ object ["verbals" .= vs]
  where
    selectVerbals =
      select $
        from $ \(v `InnerJoin` u) -> do
        on $ v ^. VerbalUser ==. u ^. UserId
        where_ (v ^. VerbalEvent ==. val eid)
        return (v, u)

postVerbalsR :: TribeId -> EventId -> Handler ()
postVerbalsR _ _ = do
  verbal <- requireJsonBody :: Handler Verbal
  requireUserSession $ verbalUser verbal
  _ <- runDB $ insert verbal
  sendResponseStatus status201 ()

