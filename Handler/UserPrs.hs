module Handler.UserPrs where

import Import
import Helpers.Request
import Data.Aeson.Types (emptyArray)

getUserPrsR :: UserId -> Handler Value
getUserPrsR _ = do
  requireSession
  return emptyArray
