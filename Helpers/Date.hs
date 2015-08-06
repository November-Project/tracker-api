module Helpers.Date where

import ClassyPrelude.Yesod
import Data.Time (parseTimeM, TimeOfDay)
import Control.Applicative (Alternative)

parseGregorianDate :: Monad m => String -> m Day
parseGregorianDate = parseTimeM True defaultTimeLocale "%F"

parseTimeOfDay :: (Monad m, Alternative m) => String -> m TimeOfDay
parseTimeOfDay s = parse "%k:%M" s
                   <|> parse "%l:%M %p" s
                   where parse = parseTimeM True defaultTimeLocale

