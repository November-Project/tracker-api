module Helpers.Crypto where

import Import hiding (hash)
import Crypto.Random.DRBG
import Data.ByteString.Base64
import Crypto.BCrypt
import Crypto.Hash

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

validateMD5 :: Text -> Text -> Bool
validateMD5 p s = encodeUtf8 p == encodeUtf8 (encryptMD5 s)

encryptMD5 :: Text -> Text
encryptMD5 = decodeUtf8 . digestToHexByteString . md5 . encodeUtf8

md5 :: ByteString -> Digest MD5
md5 = hash
