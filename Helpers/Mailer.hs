module Helpers.Mailer where

import Import hiding (urlEncode)
import System.Environment (getEnv)
import Network.HTTP (urlEncode)
import Network.Mail.SMTP

sendForgotEmail :: Text -> Text -> Handler ()
sendForgotEmail t e = do
  url <- liftIO $ getEnv "RESET_PASSWORD_URL"
  let mail = simpleMail from recipient [] [] subject $ body url
  u <- liftIO $ getEnv "SENDGRID_USERNAME"
  p <- liftIO $ getEnv "SENDGRID_PASSWORD"
  liftIO $ sendMailWithLogin' "smtp.sendgrid.net" 587 u p mail
  where
    recipient = [Address Nothing e]
    from = Address (Just "NP Tracker") "tracking+noreply@november-project.com"
    subject = "November Project Tracking App - Password Reset"
    body url = pure $ plainTextPart $ unlines
      [ "Please click on this link to reset your password:"
      , ""
      , pack $ url ++ "?reset_token=" ++ (urlEncode $ unpack $ fromStrict $ t)
      ]

