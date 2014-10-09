{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Workout where

import Import

getWorkoutR :: WorkoutId -> Handler Value
getWorkoutR wid = do
  workout <- runDB $ get404 wid
  return $ object ["workout" .= Entity wid workout]

putWorkoutR :: WorkoutId -> Handler ()
putWorkoutR wid = do
  workout <- requireJsonBody :: Handler Workout
  runDB $ replace wid workout
  sendResponseStatus status200 ("UPDATED" :: Text)
