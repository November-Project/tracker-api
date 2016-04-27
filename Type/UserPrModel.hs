{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.UserPrModel where

import Import

type UserPrModel = (Entity Result, Entity Event, Maybe (Entity Workout), Maybe (Entity Location))

instance ToJSON UserPrModel where
  toJSON (r, e, w, l) = object
    [ "event"            .= e
    , "workout"          .= w
    , "location"         .= l
    , "result"           .= r
    ]
