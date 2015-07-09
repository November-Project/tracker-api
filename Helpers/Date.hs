module Helpers.Date where

import ClassyPrelude.Yesod
import Data.Time (TimeOfDay(..))
import Text.Read (read)

parseGregorianDate :: [String] -> Day
parseGregorianDate [y,m,d] = fromGregorian (read y) (read m) (read d)
parseGregorianDate _ = fromGregorian 1988 1 3

parseTimeOfDay :: [String] -> TimeOfDay
parseTimeOfDay [h,m] = TimeOfDay (read h) (read m) 0
parseTimeOfDay _ = TimeOfDay 6 30 0

