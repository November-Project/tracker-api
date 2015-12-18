{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Type.ErrorMessage where

import Import

data ErrorMessage = ErrorMessage
    { message :: Text
    } deriving (Generic, ToJSON)
