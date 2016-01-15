module Helpers.TimeZone
  ( localTimeForTribe
  ) where

import Import
import Data.Time
import Data.Time.Zones
import Data.Time.Zones.All (tzByLabel, tzByName, TZLabel(America__New_York))

localTimeForTribe :: TribeId -> Handler LocalTime
localTimeForTribe tid = do
  tribe <- runDB $ get tid
  now <- liftIO getCurrentTime
  let timezone = fromMaybe defaultTimeZone ((tzByName . encodeUtf8) =<< tribeTimezone <$> tribe)
  return $ utcToLocalTimeTZ timezone now
  where
    defaultTimeZone = tzByLabel America__New_York

