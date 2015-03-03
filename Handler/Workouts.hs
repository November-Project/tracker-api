module Handler.Workouts where

import Import
import Helpers.Request

getWorkoutsR :: TribeId -> Handler Value
getWorkoutsR tid = do
  requireSession
  workouts <- runDB $ selectList [WorkoutTribe ==. tid] [] :: Handler [Entity Workout]
  return $ object ["workouts" .= workouts]

postWorkoutsR :: TribeId -> Handler Value
postWorkoutsR tid = do
  requireTribeAdmin tid
  w   <- requireJsonBody :: Handler Workout
  wid <- runDB $ insert w
  return $ object ["workout" .= Entity wid w]

