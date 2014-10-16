{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventModel where

import Yesod
import Data.Maybe
import Model

type EventModel = (Entity Event, Maybe (Entity Workout), Maybe (Entity Location))

instance ToJSON EventModel where
  toJSON (Entity eid e, w, l) = object
    [ "id"        .= eid
    , "date"      .= eventDate e
    , "workout"   .= toJSON w
    , "location"  .= toJSON l
    ]

