{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.UserPrModel where

import Import

type UserPrTuple = (Entity Result, Entity Event, Maybe (Entity Workout), Maybe (Entity Location))
data UserPrModel = UserPrModel (Entity Result) (Entity Event) (Maybe (Entity Workout)) (Maybe (Entity Location))

instance ToJSON UserPrModel where
  toJSON (UserPrModel r e w l) = object
    [ "event"            .= e
    , "workout"          .= w
    , "location"         .= l
    , "result"           .= r
    ]
