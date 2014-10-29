module Handler.Users where

import Import

postUsersR :: Handler ()
postUsersR = do
  user <- requireJsonBody :: Handler User
  _    <- runDB $ insert user
  sendResponseStatus status201 ("CREATED" :: Text)
