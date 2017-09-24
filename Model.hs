{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Data.Aeson ((.:?), (.!=))
import Data.Time (TimeOfDay(..))
import Type.Tag

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
  toJSON (Entity uid u) = object
    [ "id"              .= uid
    , "name"            .= userName u
    , "email"           .= userEmail u
    , "gender"          .= userGender u
    , "tribe_id"        .= userTribe u
    , "photo_url"       .= userPhotoUrl u
    , "facebook_id"     .= userFacebookId u
    , "accepted_terms"  .= userAcceptedTerms u
    , "is_verified"     .= userIsVerified u
    , "is_admin"        .= userIsAdmin u
    , "tribe_admin_id"  .= userTribeAdmin u
    ]

instance FromJSON User where
  parseJSON (Object o) = User
    <$> o .: "name"
    <*> o .: "email"
    <*> o .:? "password" .!= Nothing
    <*> o .: "gender"
    <*> o .: "tribe_id"
    <*> o .:? "photo_url" .!= Nothing
    <*> o .:? "facebook_id" .!= Nothing
    <*> o .: "accepted_terms"
    <*> pure Nothing
    <*> pure False
    <*> pure Nothing
    <*> pure False
    <*> pure Nothing
    <*> pure True

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
    , "tribe_id"  .= locationTribe l
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

instance ToJSON Tag where
  toJSON (Tag slug) = String slug

instance FromJSON Tag where
  parseJSON (String s) = pure $ Tag s
  parseJSON _          = mzero

instance ToJSON (Entity Event) where
  toJSON (Entity eid e) = object
    [ "id"                .= eid
    , "title"             .= eventTitle e
    , "tribe_id"          .= eventTribe e
    , "date"              .= eventDate e
    , "times"             .= eventTimes e
    , "hide_workout"      .= eventHideWorkout e
    , "location_id"       .= eventLocation e
    , "workout_id"        .= eventWorkout e
    , "tags"              .= eventTags e
    ]

instance FromJSON Event where
  parseJSON (Object o) = Event
    <$> o .:? "title" .!= Nothing
    <*> o .: "tribe_id"
    <*> o .: "date"
    <*> o .: "times"
    <*> o .: "hide_workout"
    <*> o .:? "location_id" .!= Nothing
    <*> o .:? "workout_id" .!= Nothing
    <*> o .:? "tags" .!= []

  parseJSON _ = mzero

instance ToJSON (Entity Recurring) where
  toJSON (Entity rid r) = object
    [ "id"                .= rid
    , "title"             .= recurringTitle r
    , "tribe_id"          .= recurringTribe r
    , "times"             .= recurringTimes r
    , "week"              .= recurringWeek r
    , "days"              .= recurringDays r
    , "hide_workout"      .= recurringHideWorkout r
    , "location_id"       .= recurringLocation r
    , "workout_id"        .= recurringWorkout r
    , "tags"              .= recurringTags r
    ]

instance FromJSON Recurring where
  parseJSON (Object o) = Recurring
    <$> o .:? "title" .!= Nothing
    <*> o .: "tribe_id"
    <*> o .: "times"
    <*> o .: "week"
    <*> o .: "days"
    <*> o .: "hide_workout"
    <*> o .:? "location_id" .!= Nothing
    <*> o .:? "workout_id" .!= Nothing
    <*> o .:? "tags" .!= []

  parseJSON _ = mzero

instance FromJSON Verbal where
  parseJSON (Object o) = Verbal
    <$> o .: "user_id"
    <*> o .: "date"
    <*> o .: "tribe"

  parseJSON _ = mzero

instance FromJSON Result where
  parseJSON (Object o) = Result
    <$> o .: "user_id"
    <*> o .: "event_id"
    <*> o .: "event_time"
    <*> o .: "reps"
    <*> o .: "time"
    <*> o .: "pr"

  parseJSON _ = mzero

instance ToJSON (Entity Result) where
  toJSON (Entity rid r) = object
    [ "id"          .= rid
    , "user_id"     .= resultUser r
    , "event_id"    .= resultEvent r
    , "event_time"  .= resultEventTime r
    , "reps"        .= resultReps r
    , "time"        .= resultTime r
    , "pr"          .= resultPr r
    ]
