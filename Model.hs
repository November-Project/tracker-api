module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Prelude

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity Tribe) where
  toJSON (Entity tid t) = object
    [ "id"          .= tid
    , "title"       .= tribeTitle t
    , "daysOfWeek"  .= tribeDaysOfWeek t
    , "latitude"    .= tribeLatitude t
    , "longitude"   .= tribeLongitude t
    , "timezone"    .= tribeTimezone t
    ]

instance ToJSON (Entity Location) where
  toJSON (Entity lid l) = object
    [ "id"        .= lid
    , "title"     .= locationTitle l
    , "latitude"  .= locationLatitude l
    , "longitude" .= locationLongitude l
    , "standard"  .= locationStandard l
    , "tribe"     .= locationTribe l
    ]
