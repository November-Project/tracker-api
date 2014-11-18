module Handler.UserMe where

import Import
import Helpers.Request

getUserMeR :: Handler Value
getUserMeR = do
  u <- getUserFromSession
  return $ object ["user" .= u]
