module Handler.UserPrs where

import Import hiding (on, (==.))
import Database.Esqueleto hiding (Value)
import Helpers.Request
import Type.UserPrModel

getUserPrsR :: UserId -> Handler Value
getUserPrsR uid = do
  requireSession

  prs <- runDB findPrs :: Handler [UserPrTuple]
  let models = fmap (\(r, e, w, l)-> UserPrModel r e w l) prs
  return $ object ["prs" .= models]
  where
    findPrs = do
      select $
        from $ \(result `InnerJoin` event `InnerJoin` workout `InnerJoin` location) -> do
          distinctOn [don (event ^. EventWorkout), don (event ^. EventLocation)] $ do
            on $ event ^. EventLocation ==. location ?. LocationId
            on $ event ^. EventWorkout ==. workout ?. WorkoutId
            on $ result ^. ResultEvent ==. event ^. EventId
            where_ $ result ^. ResultUser ==. val uid
            where_ $ workout ?. WorkoutStandard ==. val (Just True)
            where_ $ workout ?. WorkoutAllowUserPr ==. val (Just True)
            orderBy
              [ asc (event ^.EventWorkout)
              , asc (event ^. EventLocation)
              , desc (result ^. ResultReps)
              , asc (case_
                [ when_ (result ^. ResultTime ==. val 0) then_ (val maxBound) ]
                (else_ $ result ^. ResultTime))
              ]
          return (result, event, workout, location)
