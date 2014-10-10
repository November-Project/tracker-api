module Handler.Workouts where

import Import

getWorkoutsR :: TribeId -> Handler Value
getWorkoutsR tid = do
  workouts <- runDB $ selectList [WorkoutTribe ==. tid] [] :: Handler [Entity Workout]
  return $ object ["workouts" .= workouts]

postWorkoutsR :: TribeId -> Handler ()
postWorkoutsR _ = do
  workout <- requireJsonBody :: Handler Workout
  _       <- runDB $ insert workout
  sendResponseStatus status201 ("CREATED" :: Text)

