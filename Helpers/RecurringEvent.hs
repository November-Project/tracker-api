module Helpers.RecurringEvent 
  ( doesScheduleConflict
  ) where

import ClassyPrelude.Yesod hiding (intersect)
import Data.List (intersect)

doesScheduleConflict :: (Int, [Int]) -> (Int, [Int]) -> Bool
doesScheduleConflict (wl, dsl) (wr, dsr) = wl == wr && (not $ null $ dsl `intersect` dsr)

