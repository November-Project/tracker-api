{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.RecurringModel where

import Import hiding (Value)

type RecurringTuple = (Entity Recurring, Maybe (Entity Workout), Maybe (Entity Location))
data RecurringModel = RecurringModel (Entity Recurring) (Maybe (Entity Workout)) (Maybe (Entity Location))

instance ToJSON RecurringModel where
  toJSON (RecurringModel (Entity rid r) w l) = object
    [ "id"                .= rid
    , "title"             .= recurringTitle r
    , "tribe_id"          .= recurringTribe r
    , "times"             .= recurringTimes r
    , "week"              .= recurringWeek r
    , "days"              .= recurringDays r
    , "hide_workout"      .= recurringHideWorkout r
    , "workout"           .= w
    , "location"          .= l
    ]

