module Handler.Leaders where

import Import
import Helpers.Request

getLeadersR :: TribeId -> Handler Value
getLeadersR tid = do
  requireAdmin
  users <- runDB $ selectList [UserTribeAdmin ==. Just tid] [] :: Handler [Entity User]
  return $ object ["users" .= users]

