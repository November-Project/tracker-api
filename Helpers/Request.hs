module Helpers.Request
  ( siteCors
  , requireSession
  , requireUserSession
  , requireTribeAdmin
  , requireAdmin
  ) where

import Import
import Network.HTTP.Types (HeaderName)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (unless)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors

lookupUtf8Header :: HeaderName -> Handler (Maybe Text)
lookupUtf8Header headerName = return . fmap decodeUtf8 =<< lookupHeader headerName

siteCorsResourcePolicy :: CorsResourcePolicy
siteCorsResourcePolicy = CorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = simpleMethods
  , corsRequestHeaders = ["Content-Type"]
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
  t <- lookupUtf8Header "SessionToken" `orElse` notAuthenticated
  _ <- (runDB $ getBy $ UniqueSessionToken t) `orElse` notAuthenticated
  return ()
  
requireUserSession :: UserId -> Handler ()
requireUserSession uid = do
  t <- lookupUtf8Header "SessionToken" `orElse` notAuthenticated
  Entity _ s <- (runDB $ getBy $ UniqueSessionToken t) `orElse` notAuthenticated
  unless (sessionUser s == uid) $ permissionDenied ""

requireTribeAdmin :: TribeId -> Handler ()
requireTribeAdmin tid = do
  t <- lookupUtf8Header "SessionToken" `orElse` notAuthenticated
  Entity _ s <- (runDB $ getBy $ UniqueSessionToken t) `orElse` notAuthenticated
  u <- (runDB $ get $ sessionUser s) `orElse` notAuthenticated
  unless (userTribeAdmin u == Just tid || userIsAdmin u) $ permissionDenied ""

requireAdmin :: Handler ()
requireAdmin = do
  t <- lookupUtf8Header "SessionToken" `orElse` notAuthenticated
  Entity _ s <- (runDB $ getBy $ UniqueSessionToken t) `orElse` notAuthenticated
  u <- (runDB $ get $ sessionUser s) `orElse` notAuthenticated
  unless (userIsAdmin u) $ permissionDenied ""

orElse :: (Handler (Maybe a)) -> Handler a -> Handler a
a `orElse` f = do
  mresult <- a
  case mresult of
    Nothing -> f
    Just result -> return result

