module Model where

import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Prelude
import Control.Applicative
import Control.Monad

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

instance FromJSON Location where
  parseJSON (Object o) = Location
    <$> o .: "title"
    <*> o .: "latitude"
    <*> o .: "longitude"
    <*> o .: "standard"
    <*> o .: "tribe_id"
  
  parseJSON _ = mzero

instance ToJSON (Entity Workout) where
  toJSON (Entity wid w) = object
    [ "id"              .= wid
    , "title"           .= workoutTitle w
    , "description"     .= workoutDescription w
    , "reps"            .= workoutReps w
    , "time"            .= workoutTime w
    , "standard"        .= workoutStandard w
    , "allow_user_reps" .= workoutAllowUserReps w
    , "allow_user_time" .= workoutAllowUserTime w
    , "tribe_id"        .= workoutTribe w
    ]
    
instance FromJSON Workout where
  parseJSON (Object o) = Workout
    <$> o .: "title"
    <*> o .: "description"
    <*> o .: "reps"
    <*> o .: "time"
    <*> o .: "standard"
    <*> o .: "allow_user_reps"
    <*> o .: "allow_user_time"
    <*> o .: "tribe_id"
  
  parseJSON _ = mzero

