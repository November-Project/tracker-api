module Handler.Forgot where

import Import
import Helpers.Crypto
import Helpers.Mailer
import Control.Monad (mzero)

postForgotR :: Handler ()
postForgotR = do
  f <- requireJsonBody :: Handler ForgotData
  Entity uid _ <- runDB $ getBy404 $ UniqueUserEmail $ email f
  ft <- liftIO $ getRandomToken 32
  runDB $ update uid [UserForgotToken =. Just ft]
  sendForgotEmail ft $ email f
  sendResponseStatus status200 ()

data ForgotData = ForgotData { email :: Text }

instance FromJSON ForgotData where
  parseJSON (Object o) = ForgotData <$> o .: "email"
  parseJSON _ = mzero
