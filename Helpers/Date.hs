module Helpers.Date where

import ClassyPrelude.Yesod
import Data.Time (parseTimeM, TimeOfDay)

parseGregorianDate :: Monad m => String -> m Day
parseGregorianDate = parseTimeM True defaultTimeLocale "%F"

parseTimeOfDay :: Monad m => String -> m TimeOfDay
parseTimeOfDay = parseTimeM True defaultTimeLocale "%R"

