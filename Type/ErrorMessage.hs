module Type.ErrorMessage where

import Import

data ErrorMessage = ErrorMessage
    { message :: Text
    } deriving (Show)

instance ToJSON ErrorMessage where
  toJSON e = object
    [ "message" .= message e
    ]
