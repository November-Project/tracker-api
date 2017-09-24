module Handler.SpecialEvent where

import Import
import Database.Persist.Sql (rawSql)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Type.Tag
import Type.EventModel
import Type.EventResultsModel
import Type.ResultUser
import Data.Maybe (fromJust)

getSpecialEventR :: Tag -> Handler Value
getSpecialEventR (Tag tag) = do
  requireSession
  events <- runDB $ rawSql sqlExp [searchTag] :: Handler [EventTuple]
  eventResults <- forM events getResultsForEvent :: Handler [EventResultsTuple]
  let models = fmap (\(e, w, l, rs)-> EventResultsModel e w l $ fmap (uncurry ResultUserModel) rs) eventResults
  return $ object ["events" .= models]
  where
    sqlExp = "SELECT DISTINCT ON (events.tribe) ??, ??, ?? \
      FROM \"events\", \"workouts\", \"locations\" \
      WHERE \
        events.date < now() AND \
        events.tags LIKE ? AND \
        events.location = locations.id AND \
        events.workout = workouts.id \
      ORDER BY events.tribe, events.date DESC"
    searchTag = PersistText $ "%\"" <> tag <> "\"%"

getResultsForEvent :: EventTuple -> Handler EventResultsTuple
getResultsForEvent (e, w, l) = do
  results <- runDB selectResults
  return (e, fromJust w, fromJust l, results)
  where
    selectResults =
      ES.select $
        ES.from $ \(r `ES.InnerJoin` u) -> do
        ES.on $ r ES.^. ResultUser ES.==. u ES.^. UserId
        ES.where_ (r ES.^. ResultEvent ES.==. ES.val (entityKey e))
        return (r, u)
