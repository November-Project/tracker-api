module Handler.Users where

import Import hiding (concat)
import Helpers.Request
import Data.Text (concat)

getUsersR :: Handler Value
getUsersR = do
  requireAnyAdmin

  term <- lookupGetParam "q"
  tribe <- lookupGetParam "tribe"
  let filters = case (term, fromPathPiece =<< tribe) of
                  (Just searchTerm, Just tribeID) -> Just $ searchFilters searchTerm ++ tribeFilter tribeID
                  (Just searchTerm, Nothing) -> Just $ searchFilters searchTerm
                  (Nothing, Just tribeID) -> Just $ tribeFilter tribeID
                  (_, _) -> Nothing

  users <- maybe (pure []) userSearch filters
  return $ object ["users" .= users]
  where
    ilike field val = Filter field (Left $ concat ["%", val, "%"]) (BackendSpecificFilter "ILIKE")
    userSearch filters = runDB $ selectList (filters ++ [UserAcceptedTerms ==. True]) [] :: Handler [Entity User]
    searchFilters t = [UserEmail `ilike` t] ||. [UserName `ilike` t]
    tribeFilter t = [UserTribe ==. t]
