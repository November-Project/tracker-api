{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventResultsModel where

import Import hiding (Value)
import Type.ResultUser

type EventResultsTuple = (Entity Event, Entity Workout, Entity Location, [ResultUserTuple])
data EventResultsModel = EventResultsModel (Entity Event) (Entity Workout) (Entity Location) ([ResultUserModel])

instance ToJSON EventResultsModel where
  toJSON (EventResultsModel (Entity eid e) w l rs) = object
    [ "id"                .= eid
    , "title"             .= eventTitle e
    , "tribe_id"          .= eventTribe e
    , "date"              .= eventDate e
    , "times"             .= eventTimes e
    , "hide_workout"      .= eventHideWorkout e
    , "tags"              .= eventTags e
    , "workout"           .= w
    , "location"          .= l
    , "results"           .= rs
    ]

