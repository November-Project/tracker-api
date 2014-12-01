module Handler.Result where

import Import
import Helpers.Request

putResultR :: TribeId -> EventId -> ResultId -> Handler ()
putResultR _ _ rid = do
  r <- requireJsonBody :: Handler Result
  requireUserSession $ resultUser r
  runDB $ update rid
    [ ResultReps  =. resultReps r
    , ResultTime  =. resultTime r
    , ResultPr    =. resultPr r
    ]
  sendResponseStatus status200 ()

