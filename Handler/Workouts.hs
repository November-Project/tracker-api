{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Workouts where

import Import

getWorkoutsR :: Handler Value
getWorkoutsR = do
  workouts <- runDB $ selectList [] [] :: Handler [Entity Workout]
  return $ object ["workouts" .= workouts]

postWorkoutsR :: Handler ()
postWorkoutsR = do
  workout <- requireJsonBody :: Handler Workout
  _       <- runDB $ insert workout

  sendResponseStatus status201 ("CREATED" :: Text)

