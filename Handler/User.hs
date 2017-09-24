module Handler.User where

import Import
import Helpers.Request
import Type.RestrictedUser

getUserR :: UserId -> Handler Value
getUserR uid = do
  requireSession
  u <- runDB $ get404 uid
  return $ object ["user" .= (RestrictedUser $ Entity uid u)]

putUserR :: UserId -> Handler Value
putUserR uid = do
  requireUserSession uid
  u <- requireJsonBody :: Handler User
  runDB $ update uid
    [ UserName          =. userName u
    , UserGender        =. userGender u
    , UserPhotoUrl      =. userPhotoUrl u
    , UserTribe         =. userTribe u
    , UserAcceptedTerms =. userAcceptedTerms u
    ]
  return $ object ["user" .= Entity uid u]
