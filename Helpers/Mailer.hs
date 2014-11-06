module Helpers.Mailer where

import Import
import System.Environment (getEnv)
import Network.HTTP (urlEncode)
import Network.Mail.SMTP
import Data.Text.Lazy (pack, unpack, fromStrict)
import qualified Data.Text.Lazy as TL

sendForgotEmail :: Text -> Text -> Handler ()
sendForgotEmail t e = do
  url <- liftIO $ getEnv "RESET_PASSWORD_URL"
  let mail = simpleMail from recipient [] [] subject $ body url
  u <- liftIO $ getEnv "SENDGRID_USERNAME"
  p <- liftIO $ getEnv "SENDGRID_PASSWORD"
  liftIO $ sendMailWithLogin "smtp.sendgrid.net" u p mail
  where
    recipient = [Address Nothing e]
    from = Address (Just "NP Tracker") "tracking+noreply@november-project.com"
    subject = "November Project Tracking App - Password Reset"
    body url = pure $ plainTextPart $ TL.unlines
      [ "Please click on this link to reset your password:"
      , ""
      , pack $ url ++ (urlEncode $ unpack $ fromStrict $ t)
      ]

