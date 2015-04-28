module Handler.Tribes where

import Import
import Helpers.Request

getTribesR :: Handler Value
getTribesR = do
  tribes <- runDB $ selectList [] [] :: Handler [Entity Tribe]
  return $ object ["tribes" .= tribes]

postTribesR :: Handler Value
postTribesR = do
  requireAdmin

  t <- requireJsonBody :: Handler Tribe
  tid <- runDB $ insert t
  return $ object ["tribe" .= Entity tid t]
  
