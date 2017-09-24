module Handler.Verbals where

import Import hiding ((==.), on)
import Database.Esqueleto hiding (Value)
import Helpers.Date
import Helpers.Request
import Type.ErrorMessage
import Type.VerbalUser

getVerbalsR :: TribeId -> Handler Value
getVerbalsR tid = do
  requireSession
  
  dateString <- lookupGetParam "date"
  let date = unpack <$> dateString >>= parseGregorianDate

  case date of
    Nothing -> sendResponseStatus status400 $ toJSON $ ErrorMessage "Date parameter required."
    Just d -> do
      vs <- runDB $ selectVerbals d :: Handler [VerbalUserTuple]
      let models = fmap (uncurry VerbalUserModel) vs
      return $ object ["verbals" .= models]
  where
    selectVerbals d =
      select $
        from $ \(v `InnerJoin` u) -> do
        on $ v ^. VerbalUser ==. u ^. UserId
        where_ (v ^. VerbalDate ==. val d)
        where_ (v ^. VerbalTribe ==. val tid)
        return (v, u)

postVerbalsR :: TribeId -> Handler Value
postVerbalsR _  = do
  verbal <- requireJsonBody :: Handler Verbal
  requireUserSession $ verbalUser verbal
  user <- runDB $ get404 $ verbalUser verbal
  vid <- runDB $ insert verbal
  return $ object ["verbal" .= (VerbalUserModel (Entity vid verbal) (Entity (verbalUser verbal) user))]

