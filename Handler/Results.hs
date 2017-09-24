module Handler.Results where

import Import hiding ((==.), on)
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.ResultUser

getResultsR :: TribeId -> EventId -> Handler Value
getResultsR _ eid = do
  requireSession
  rs <- runDB selectResults :: Handler [ResultUserTuple]
  let models = fmap (\(r, u)-> ResultUserModel r u) rs
  return $ object ["results" .= models]
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
  let uid = resultUser r
  requireUserSession uid

  rid <- runDB $ insert r
  u <- runDB $ get404 uid

  let result = ResultUserModel (Entity rid r) (Entity uid u)
  sendResponseStatus status201 $ object ["result" .= result]

