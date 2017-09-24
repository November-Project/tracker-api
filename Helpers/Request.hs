module Helpers.Request
  ( siteCors
  , requireSession
  , requireUserSession
  , requireTribeAdmin
  , requireAdmin
  , requireAnyAdmin
  , getUserFromSession
  ) where

import Import
import Control.Monad.Except (runExceptT)
import Control.Lens ((^.), (^?))
import Crypto.JWT
import qualified Data.ByteString.Lazy as L
import Data.Aeson (decode)
import Database.Persist.Sql (toSqlKey)
import Network.HTTP.Conduit (simpleHttp)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors

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
requireSession = void getClaimsFromHeader

requireUserSession :: UserId -> Handler ()
requireUserSession uid = do
  Entity i _ <- getUserFromSession
  unless (i == uid) $ permissionDenied ""

requireTribeAdmin :: TribeId -> Handler ()
requireTribeAdmin tid = do
  Entity _ u <- getUserFromSession
  unless (userTribeAdmin u == Just tid || userIsAdmin u) $ permissionDenied ""

requireAdmin :: Handler ()
requireAdmin = do
  Entity _ u <- getUserFromSession
  unless (userIsAdmin u) $ permissionDenied ""

requireAnyAdmin :: Handler ()
requireAnyAdmin = do
  Entity _ u <- getUserFromSession
  unless (userIsAdmin u || (isJust $ userTribeAdmin u)) $ permissionDenied ""

getClaimsFromHeader :: Handler ClaimsSet
getClaimsFromHeader = do
  t <- L.fromStrict <$> lookupHeader "AUTHORIZATION" `orElse` notAuthenticated
  jwks <- liftIO $ simpleHttp "https://novproject.auth0.com/.well-known/jwks.json"
  maybe notAuthenticated (\k-> do
    result <- runExceptT $ verifyClaims (defaultJWTValidationSettings (== "bob")) (k :: JWKSet) =<< (decodeCompact t)
    case result of
      Left e -> print (e :: JWTError) >> notAuthenticated
      Right claims -> return claims
    ) $ decode jwks

getUserIdFromClaims :: Handler UserId
getUserIdFromClaims = do
  claims <- getClaimsFromHeader
  let muid = readMay =<< (^? string) =<< claims ^. claimSub :: Maybe Int64
  maybe (permissionDenied "") (return . toSqlKey) muid

getUserFromSession :: Handler (Entity User)
getUserFromSession = do
  uid <- getUserIdFromClaims
  mu <- runDB $ get uid
  maybe notAuthenticated (return . Entity uid) mu

orElse :: (Handler (Maybe a)) -> Handler a -> Handler a
a `orElse` f = do
  mresult <- a
  case mresult of
    Nothing -> f
    Just result -> return result

