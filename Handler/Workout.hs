module Handler.Workout where

import Import

getWorkoutR :: TribeId -> WorkoutId -> Handler Value
getWorkoutR _ wid = do
  workout <- runDB $ get404 wid
  return $ object ["workout" .= Entity wid workout]

putWorkoutR :: TribeId -> WorkoutId -> Handler ()
putWorkoutR _ wid = do
  workout <- requireJsonBody :: Handler Workout
  runDB $ replace wid workout
  sendResponseStatus status200 ("UPDATED" :: Text)
