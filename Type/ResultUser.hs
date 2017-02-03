{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.ResultUser where

import Import

type ResultUser = (Entity Result, Entity User)

instance ToJSON ResultUser where
  toJSON (Entity rid r, Entity uid u) = object
    [ "id"              .= rid
    , "user_id"         .= uid
    , "user_name"       .= userName u
    , "user_photo_url"  .= userPhotoUrl u
    , "user_gender"     .= userGender u
    , "event_id"        .= resultEvent r
    , "event_time"      .= resultEventTime r
    , "reps"            .= resultReps r
    , "time"            .= resultTime r
    , "pr"              .= resultPr r
    ]
