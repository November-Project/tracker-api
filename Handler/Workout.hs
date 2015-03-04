module Handler.Workout where

import Import
import Helpers.Request

getWorkoutR :: TribeId -> WorkoutId -> Handler Value
getWorkoutR _ wid = do
  requireSession
  workout <- runDB $ get404 wid
  return $ object ["workout" .= Entity wid workout]

putWorkoutR :: TribeId -> WorkoutId -> Handler Value
putWorkoutR tid wid = do
  requireTribeAdmin tid
  w <- requireJsonBody :: Handler Workout
  runDB $ replace wid w
  return $ object ["workout" .= Entity wid w]

