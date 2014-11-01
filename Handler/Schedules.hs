module Handler.Schedules where

import Import
import Helpers.Request

getSchedulesR :: TribeId -> Handler Value
getSchedulesR tid = do
  requireSession

  schedules <- runDB $ selectList [ScheduleTribe ==. tid] [] :: Handler [Entity Schedule]
  return $ object ["schedules" .= schedules]
