module Handler.User where

import Import

putUserR :: UserId -> Handler ()
putUserR _ = sendResponseStatus status200 ()
