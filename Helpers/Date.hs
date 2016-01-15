module Helpers.Date
  ( parseGregorianDate
  , parseTimeOfDay
  , weekOfMonth
  , weekOfMonth'
  , dayOfWeek
  ) where 

import ClassyPrelude.Yesod
import Data.Time
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

parse :: (Monad m, ParseTime t) => String -> String -> m t
parse = parseTimeM True defaultTimeLocale

parseGregorianDate :: Monad m => String -> m Day
parseGregorianDate = parse "%F"

parseTimeOfDay :: (Monad m, Alternative m) => String -> m TimeOfDay
parseTimeOfDay s = parse "%k:%M" s <|> parse "%l:%M %p" s

weekOfMonth :: Day -> Int
weekOfMonth day
  | toMonth lastWeek /= month = 1
  | otherwise = (1+) $ weekOfMonth lastWeek 
  where 
    month = toMonth day
    lastWeek = addDays (-7) day

weekOfMonth' :: Day -> Int
weekOfMonth' day
  | toMonth nextWeek /= month = -1
  | otherwise = (-1+) $ weekOfMonth' nextWeek 
  where 
    month = toMonth day
    nextWeek = addDays 7 day

dayOfWeek :: Day -> Int
dayOfWeek = snd . sundayStartWeek

toMonth :: Day -> Int
toMonth = middle . toGregorian
  where middle (_, x, _) = x

