{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.VerbalUser where

import Import

type VerbalUser = (Entity Verbal, Entity User)

instance ToJSON VerbalUser where
  toJSON (Entity vid v, Entity uid u) = object
    [ "id"        .= vid
    , "user_id"   .= uid
    , "user_name" .= userName u
    , "event_id"  .= verbalEvent v
    ]

