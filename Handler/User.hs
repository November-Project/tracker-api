module Handler.User where

import Import
import Helpers.Request

putUserR :: UserId -> Handler Value
putUserR uid = do
  requireUserSession uid
  u <- requireJsonBody :: Handler User
  runDB $ update uid
    [ UserTribe         =. userTribe u
    , UserAcceptedTerms =. userAcceptedTerms u
    ]
  return $ object ["user" .= Entity uid u]
