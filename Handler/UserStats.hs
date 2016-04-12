module Handler.UserStats where

import Import
import Helpers.Request

getUserStatsR :: UserId -> Handler Value
getUserStatsR uid = do
  requireSession

  stats <- getUserStats
  return $ object ["stats" .=
      object ["verbals" .= fst stats, "results" .= snd stats]]
  where
    getUserStats = do
      verbals <- runDB $ selectList [VerbalUser ==. uid] []
      results <- runDB $ selectList [ResultUser ==. uid] []
      return (length verbals, length results)
