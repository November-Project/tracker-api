module Handler.Tribe where

import Import
import Helpers.Request

getTribeR :: TribeId -> Handler Value
getTribeR tid = do
  t <- runDB $ get404 tid
  return $ object ["tribe" .= Entity tid t]

putTribeR :: TribeId -> Handler Value
putTribeR tid = do
  requireAdmin
  t <- requireJsonBody :: Handler Tribe
  runDB $ replace tid t
  return $ object ["tribe" .= Entity tid t]

