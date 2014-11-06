{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.EventFullModel where

import Import
import Type.EventModel
import Type.VerbalUser
import Type.ResultUser
import Database.Esqueleto (unValue)

type EventFullModel = (EventModel, [VerbalUser], [ResultUser])

instance ToJSON EventFullModel where
  toJSON ((Entity eid e, w, l, vc, rc), v, r) = object
    [ "id"            .= eid 
    , "date"          .= eventDate e
    , "workout"       .= w
    , "location"      .= l
    , "verbal_count"  .= unValue vc
    , "result_count"  .= unValue rc
    , "verbals"       .= v
    , "results"       .= r
    ]
