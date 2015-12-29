module Helpers.Date
  ( parseGregorianDate
  , parseTimeOfDay
  , recurringDays
  ) where 

import ClassyPrelude.Yesod
import Data.Time
import Data.Time.Calendar.OrdinalDate (sundayStartWeek)

parseGregorianDate :: Monad m => String -> m Day
parseGregorianDate = parseTimeM True defaultTimeLocale "%F"

parseTimeOfDay :: (Monad m, Alternative m) => String -> m TimeOfDay
parseTimeOfDay s = parse "%k:%M" s
                   <|> parse "%l:%M %p" s
                   where parse = parseTimeM True defaultTimeLocale

recurringDays :: [Int] -> Int -> Day -> Day -> [Day]
recurringDays daysOfWeek week startDay endDay = filterWeekFromDays week validDays
  where
    validDays = filter isValidDay allDays
    allDays = takeWhile (<=endDay) $ (`addDays` startDay) <$> [0..]
    isValidDay = (`elem` daysOfWeek) . dayOfWeek

filterWeekFromDays :: Int -> [Day] -> [Day]
filterWeekFromDays week days
  | week > 0 = filter (validateWeek . weekDayOfMonth) days
  | week < 0 = filter (validateWeek . weekDayOfMonth') days
  | otherwise = days
  where validateWeek = (==week)

weekDayOfMonth :: Day -> Int
weekDayOfMonth day
  | toMonth lastWeek /= month = 1
  | otherwise = (1+) $ weekDayOfMonth lastWeek 
  where 
    month = toMonth day
    lastWeek = addDays (-7) day

weekDayOfMonth' :: Day -> Int
weekDayOfMonth' day
  | toMonth nextWeek /= month = -1
  | otherwise = (-1+) $ weekDayOfMonth' nextWeek 
  where 
    month = toMonth day
    nextWeek = addDays 7 day

dayOfWeek :: Day -> Int
dayOfWeek = snd . sundayStartWeek

toMonth :: Day -> Int
toMonth = middle . toGregorian
  where middle (_, x, _) = x

