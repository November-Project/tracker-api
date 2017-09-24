{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.ResultUser where

import Import

type ResultUserTuple = (Entity Result, Entity User)
data ResultUserModel = ResultUserModel (Entity Result) (Entity User)

instance ToJSON ResultUserModel where
  toJSON (ResultUserModel (Entity rid r) (Entity uid u)) = object
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
