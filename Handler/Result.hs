module Handler.Result where

import Import
import Helpers.Request
import Type.ResultUser

putResultR :: TribeId -> EventId -> ResultId -> Handler Value
putResultR _ _ rid = do
  r <- requireJsonBody :: Handler Result
  let uid = resultUser r

  requireUserSession uid
  runDB $ update rid
    [ ResultReps  =. resultReps r
    , ResultTime  =. resultTime r
    , ResultPr    =. resultPr r
    ]

  u <- runDB $ get404 uid
  let result = ResultUserModel (Entity rid r) (Entity uid u)
  return $ object ["result" .= result]

deleteResultR :: TribeId -> EventId -> ResultId -> Handler ()
deleteResultR _ _ rid = do
  r <- runDB $ get404 rid
  requireUserSession $ resultUser r
  runDB $ delete rid
  sendResponseStatus status204 ()

