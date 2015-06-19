module Helpers.Request
  ( siteCors
  , requireSession
  , requireUserSession
  , requireTribeAdmin
  , requireAdmin
  , getUserFromSession
  , getSessionFromHeader
  ) where

import Import
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors

lookupUtf8Header :: HeaderName -> Handler (Maybe Text)
lookupUtf8Header headerName = return . fmap decodeUtf8 =<< lookupHeader headerName

siteCorsResourcePolicy :: CorsResourcePolicy
siteCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = simpleMethods ++ ["PUT", "DELETE"]
  , corsRequestHeaders = ["Content-Type", "AUTHORIZATION"]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

siteCors :: Middleware
siteCors = cors $ const $ Just siteCorsResourcePolicy

requireSession :: Handler ()
requireSession = do
  _ <- getSessionFromHeader
  return ()

requireUserSession :: UserId -> Handler ()
requireUserSession uid = do
  Entity _ s <- getSessionFromHeader
  unless (sessionUser s == uid) $ permissionDenied ""

requireTribeAdmin :: TribeId -> Handler ()
requireTribeAdmin tid = do
  Entity _ u <- getUserFromSession
  unless (userTribeAdmin u == Just tid || userIsAdmin u) $ permissionDenied ""

requireAdmin :: Handler ()
requireAdmin = do
  Entity _ u <- getUserFromSession
  unless (userIsAdmin u) $ permissionDenied ""

getSessionFromHeader :: Handler (Entity Session)
getSessionFromHeader = do
  t <- lookupUtf8Header "AUTHORIZATION" `orElse` notAuthenticated
  (runDB $ getBy $ UniqueSessionToken t) `orElse` notAuthenticated

getUserFromSession :: Handler (Entity User)
getUserFromSession = do
  Entity _ s <- getSessionFromHeader
  mu <- runDB $ get $ sessionUser s
  maybe notAuthenticated (\u -> return $ Entity (sessionUser s) u) mu

orElse :: (Handler (Maybe a)) -> Handler a -> Handler a
a `orElse` f = do
  mresult <- a
  case mresult of
    Nothing -> f
    Just result -> return result

