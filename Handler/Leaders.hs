module Handler.Leaders where

import Import
import Helpers.Request

getLeadersR :: TribeId -> Handler Value
getLeadersR tid = do
  requireTribeAdmin tid
  users <- runDB $ selectList [UserTribeAdmin ==. Just tid] [] :: Handler [Entity User]
  return $ object ["leaders" .= users]

