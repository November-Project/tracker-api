module Helpers.Request (allowCrossOrigin, requireTribeAdmin) where

import Import
import Network.HTTP.Types (HeaderName)
import Data.Text.Encoding (decodeUtf8)
import Control.Monad (unless)

lookupUtf8Header :: HeaderName -> Handler (Maybe Text)
lookupUtf8Header headerName = return . fmap decodeUtf8 =<< lookupHeader headerName

allowCrossOrigin :: Handler ()
allowCrossOrigin = do
    mo <- lookupUtf8Header "Origin"
    mrh <- lookupUtf8Header "Access-Control-Request-Headers"

    case mo of
        Just o  -> addHeader "Access-Control-Allow-Origin" o
        Nothing -> return ()

    case mrh of
        Just rh -> addHeader "Access-Control-Allow-Headers" rh
        Nothing -> return ()

    addHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS"
    addHeader "Access-Control-Allow-Credentials" "true"

requireTribeAdmin :: TribeId -> Handler ()
requireTribeAdmin tid = do
  token <- lookupUtf8Header "Token" `orElse` notAuthenticated
  (Entity _ s) <- runDB (getBy (UniqueSessionToken token)) `orElse` permissionDenied ""
  u <- runDB (get (sessionUser s)) `orElse` permissionDenied ""
  unless (userTribeAdmin u == Just tid) $ permissionDenied ""

orElse :: (Handler (Maybe a)) -> Handler a -> Handler a
a `orElse` f = do
  mresult <- a
  case mresult of
    Nothing -> f
    Just result -> return result

