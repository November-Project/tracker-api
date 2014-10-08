{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Workouts where

import Import

getWorkoutsR :: Handler Value
getWorkoutsR = do
  workouts <- runDB $ selectList [] [] :: Handler [Entity Tribe]
  return $ object ["workouts" .= workouts]

