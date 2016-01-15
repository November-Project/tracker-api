module Helpers.Date
  ( parseGregorianDate
  , parseTimeOfDay
  , doesScheduleConflict
  , getRecurringDays
  ) where 

import ClassyPrelude.Yesod hiding (intersect)
import Data.Time
import Data.List (intersect)
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

parseGregorianDate :: Monad m => String -> m Day
parseGregorianDate = parseTimeM True defaultTimeLocale "%F"

parseTimeOfDay :: (Monad m, Alternative m) => String -> m TimeOfDay
parseTimeOfDay s = parse "%k:%M" s
                   <|> parse "%l:%M %p" s
                   where parse = parseTimeM True defaultTimeLocale

doesScheduleConflict :: (Int, [Int]) -> (Int, [Int]) -> Bool
doesScheduleConflict (wl, dsl) (wr, dsr)
  | wl == wr = True
  | otherwise = null $ dsl `intersect` dsr

getRecurringDays :: [Int] -> Int -> Day -> Day -> [Day]
getRecurringDays daysOfWeek week startDay endDay = filterWeekFromDays week validDays
  where
    validDays = filter isValidDay allDays
    allDays = takeWhile (<=endDay) $ (`addDays` startDay) <$> [0..]
    isValidDay = (`elem` daysOfWeek) . dayOfWeek

filterWeekFromDays :: Int -> [Day] -> [Day]
filterWeekFromDays week days
  | week > 0 = filter (validateWeek . weekOfMonth) days
  | week < 0 = filter (validateWeek . weekOfMonth') days
  | otherwise = days
  where validateWeek = (==week)

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

