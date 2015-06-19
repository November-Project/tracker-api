module Handler.Results where

import Import hiding ((==.), on)
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.ResultUser

getResultsR :: TribeId -> EventId -> Handler Value
getResultsR _ eid = do
  requireSession
  rs <- runDB selectResults :: Handler [ResultUser]
  return $ object ["results" .= rs]
  where
    selectResults =
      select $
        from $ \(r `InnerJoin` u) -> do
        on $ r ^. ResultUser ==. u ^. UserId
        where_ (r ^. ResultEvent ==. val eid)
        return (r, u)

postResultsR :: TribeId -> EventId -> Handler ()
postResultsR _ _ = do
  r <- requireJsonBody :: Handler Result
  requireUserSession $ resultUser r
  _      <- runDB $ insert r
  sendResponseStatus status201 ()

