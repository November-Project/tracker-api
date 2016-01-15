{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventModel where

import Import hiding (Value)

type EventModel = (Entity Event, Maybe (Entity Workout), Maybe (Entity Location))

instance ToJSON EventModel where
  toJSON (Entity eid e, w, l) = object
    [ "id"                .= eid
    , "title"             .= eventTitle e
    , "tribe_id"          .= eventTribe e
    , "date"              .= eventDate e
    , "times"             .= eventTimes e
    , "hide_workout"      .= eventHideWorkout e
    , "workout"           .= w
    , "location"          .= l
    ]

