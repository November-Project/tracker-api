{-# OPTIONS_GHC -fno-warn-orphans #-}
module Type.VerbalUser where

import Import

type VerbalUserTuple = (Entity Verbal, Entity User)
data VerbalUserModel = VerbalUserModel (Entity Verbal) (Entity User)

instance ToJSON VerbalUserModel where
  toJSON (VerbalUserModel (Entity vid v) (Entity uid u)) = object
    [ "id"              .= vid
    , "user_id"         .= uid
    , "user_name"       .= userName u
    , "user_photo_url"  .= userPhotoUrl u
    , "date"            .= verbalDate v
    , "tribe"           .= verbalTribe v
    ]

