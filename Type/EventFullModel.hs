{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventFullModel where

import Yesod
import Model
import Type.EventModel
import Type.VerbalUser
import Type.ResultUser

type EventFullModel = (EventModel, [VerbalUser], [ResultUser])

instance ToJSON EventFullModel where
  toJSON ((Entity eid e, w, l), v, r) = object
    [ "id"        .= eid 
    , "date"      .= eventDate e
    , "workout"   .= toJSON w
    , "location"  .= toJSON l
    , "verbals"   .= toJSON v
    , "results"   .= toJSON r
    ]
