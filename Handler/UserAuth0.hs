module Handler.UserAuth0 where

import Import
import Helpers.Request

getUserAuth0R :: Handler Value
getUserAuth0R = do
  u <- getUserFromSession
  return $ object ["user" .= u]
