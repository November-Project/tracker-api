module Handler.Schedules where

import Import

getSchedulesR :: TribeId -> Handler Value
getSchedulesR tid = do
  schedules <- runDB $ selectList [ScheduleTribe ==. tid] [] :: Handler [Entity Schedule]
  return $ object ["schedules" .= schedules]
