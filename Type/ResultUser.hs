{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.ResultUser where

import Yesod
import Model

type ResultUser = (Entity Result, Entity User)

instance ToJSON ResultUser where
  toJSON (Entity rid r, Entity uid u) = object
    [ "id"        .= rid
    , "user_id"   .= uid
    , "user_name" .= userName u
    , "event_id"  .= resultEvent r
    , "reps"      .= resultReps r
    , "time"      .= resultTime r
    , "pr"        .= resultPr r
    ]
