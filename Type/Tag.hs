module Type.Tag where

import Data.Eq (Eq)
import Text.Read (Read)
import Text.Show (Show)
import Data.Text (Text)
import Database.Persist (PersistField)
import Yesod.Core.Dispatch (PathPiece)

newtype Tag = Tag Text
  deriving (Show, Read, Eq, PersistField, PathPiece)
