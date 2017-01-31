module Handler.SpecialEvent where

import Import
import Database.Persist.Sql (rawSql)
import qualified Database.Esqueleto as ES
import Helpers.Request
import Type.Tag
import Type.EventModel
import Type.EventResultsModel
import Data.Maybe (fromJust)

getSpecialEventR :: Tag -> Handler Value
getSpecialEventR (Tag tag) = do
  requireSession
  events <- runDB $ rawSql sqlExp [searchTag] :: Handler [EventModel]
  eventResults <- forM events getResultsForEvent :: Handler [EventResultsModel]
  return $ object ["events" .= eventResults]
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

getResultsForEvent :: EventModel -> Handler EventResultsModel
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
