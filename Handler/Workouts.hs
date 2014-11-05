module Handler.Workouts where

import Import
import Helpers.Request

getWorkoutsR :: TribeId -> Handler Value
getWorkoutsR tid = do
  requireSession
  workouts <- runDB $ selectList [WorkoutTribe ==. tid] [] :: Handler [Entity Workout]
  return $ object ["workouts" .= workouts]

postWorkoutsR :: TribeId -> Handler ()
postWorkoutsR tid = do
  requireTribeAdmin tid
  workout <- requireJsonBody :: Handler Workout
  _       <- runDB $ insert workout
  sendResponseStatus status201 ()

