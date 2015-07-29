{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Aeson ((.:?))
import Data.Time (showGregorian, TimeOfDay)
import Helpers.Date

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
  toJSON (Entity uid u) = object
    [ "id"        .= uid
    , "name"      .= userName u
    , "email"     .= userEmail u
    , "gender"    .= userGender u
    , "tribe_id"  .= userTribe u
    , "facebook_id" .= userFacebookId u
    , "accepted_terms"  .= userAcceptedTerms u
    , "is_verified"     .= userIsVerified u
    , "is_admin"        .= userIsAdmin u
    , "tribe_admin"     .= userTribeAdmin u
    ]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .: "name"
    <*> o .: "email"
    <*> o .:? "password"
    <*> o .: "gender"
    <*> o .: "tribe_id"
    <*> o .:? "facebook_id"
    <*> o .: "accepted_terms"
    <*> pure Nothing
    <*> pure False
    <*> pure Nothing
    <*> pure False
    <*> pure Nothing

  parseJSON _ = mzero

instance ToJSON (Entity Tribe) where
  toJSON (Entity tid t) = object
    [ "id"          .= tid
    , "title"       .= tribeTitle t
    , "days_of_week".= tribeDaysOfWeek t
    , "latitude"    .= tribeLatitude t
    , "longitude"   .= tribeLongitude t
    , "timezone"    .= tribeTimezone t
    ]

instance FromJSON Tribe where
  parseJSON (Object o) = Tribe
    <$> o .: "title"
    <*> o .: "days_of_week"
    <*> o .: "latitude"
    <*> o .: "longitude"
    <*> o .: "timezone"

  parseJSON _ = mzero

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
    , "allow_user_pr"   .= workoutAllowUserPr w
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
    <*> o .: "allow_user_pr"
    <*> o .: "tribe_id"

  parseJSON _ = mzero

instance ToJSON (Entity Event) where
  toJSON (Entity eid e) = object
    [ "id"                .= eid
    , "tribe_id"          .= eventTribe e
    , "date"              .= eventDate e
    , "times"             .= eventTimes e
    , "recurring"         .= eventRecurring e
    , "week"              .= eventWeek e
    , "days"              .= eventDays e
    , "recurring_id"      .= eventRecurringEvent e
    , "location_id"       .= eventLocation e
    , "workout_id"        .= eventWorkout e
    ]

instance FromJSON Event where
  parseJSON (Object o) = Event
    <$> o .: "tribe_id"
    <*> o .:? "date"
    <*> o .: "times"
    <*> o .: "recurring"
    <*> o .: "week"
    <*> o .: "days"
    <*> o .:? "recurring_id"
    <*> o .:? "location_id"
    <*> o .:? "workout_id"

  parseJSON _ = mzero

instance FromJSON Day where
  parseJSON (String s) = parseGregorianDate $ unpack s
  parseJSON _ = mzero

instance ToJSON Day where
  toJSON = String . pack . showGregorian

instance FromJSON TimeOfDay where
  parseJSON (String s) = parseTimeOfDay $ unpack s
  parseJSON _ = mzero

instance ToJSON TimeOfDay where
  toJSON = String . pack . show

instance FromJSON Verbal where
  parseJSON (Object o) = Verbal
    <$> o .: "user_id"
    <*> o .: "event_id"

  parseJSON _ = mzero

instance FromJSON Result where
  parseJSON (Object o) = Result
    <$> o .: "user_id"
    <*> o .: "event_id"
    <*> o .: "reps"
    <*> o .: "time"
    <*> o .: "pr"

  parseJSON _ = mzero

