module Helpers.Crypto where

import Import
import Crypto.Random.DRBG
import Data.ByteString.Base64
import Crypto.BCrypt

getRandomToken :: Int -> IO Text
getRandomToken n = do
  gen <- newGenIO :: IO HashDRBG
  case genBytes n gen of
    Right (hashCode, _) -> return $ decodeUtf8 $ encode hashCode
    Left _ -> getRandomToken n

encryptText :: Text -> IO (Maybe Text)
encryptText s = do
  e <- hashPasswordUsingPolicy fastBcryptHashingPolicy $ encodeUtf8 s
  return $ decodeUtf8 <$> e

validateText :: Text -> Text -> Bool
validateText p s = validatePassword (encodeUtf8 p) (encodeUtf8 s)
