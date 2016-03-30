module Type.RestrictedUser where

import Import

data RestrictedUser = RestrictedUser (Entity User)

instance ToJSON RestrictedUser where
  toJSON (RestrictedUser (Entity uid u)) = object
    [ "id"              .= uid
    , "name"            .= userName u
    , "gender"          .= userGender u
    , "tribe_id"        .= userTribe u
    , "photo_url"       .= userPhotoUrl u
    , "facebook_id"     .= userFacebookId u
    , "tribe_admin_id"  .= userTribeAdmin u
    ]
