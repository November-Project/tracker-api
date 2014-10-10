module Handler.Results where

import Import

getResultsR :: TribeId -> EventId -> Handler Value
getResultsR _ eid = do
  results <- runDB $ selectList [ResultEvent ==. eid] [] :: Handler [Entity Result]
  return $ object ["results" .= results]

postResultsR :: TribeId -> EventId -> Handler ()
postResultsR _ _ = do
  result <- requireJsonBody :: Handler Result
  _      <- runDB $ insert result
  sendResponseStatus status201 ("CREATED" :: Text)

