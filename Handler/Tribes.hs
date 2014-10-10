module Handler.Tribes where

import Import

getTribesR :: Handler Value
getTribesR = do
  tribes <- runDB $ selectList [] [] :: Handler [Entity Tribe]
  return $ object ["tribes" .= tribes]
  
