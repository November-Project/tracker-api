{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventResultsModel where

import Import hiding (Value)
import Type.ResultUser

type EventResultsModel = (Entity Event, Entity Workout, Entity Location, [ResultUser])

instance ToJSON EventResultsModel where
  toJSON (Entity eid e, w, l, rs) = object
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

