{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NoteTypes 
  ( NoteTitle
  , NoteBody
  , NoteRequest(..)
  , makeTitle
  , makeBody
  , noteTitleSample
  , noteBodySample
  ) where

import RIO (Text, Generic)
import Data.Validation (Validation(..))
import qualified Data.Text as T
import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import Validation (VError(..))

newtype NoteTitle = NoteTitle Text deriving (Eq, Show, Read, Generic)
newtype NoteBody = NoteBody Text deriving (Eq, Show, Read, Generic)
data NoteRequest = CreateNoteRequest | GetNoteRequest | GetNotesByNameRequest deriving Eq

instance FromJSON NoteTitle
instance ToJSON NoteTitle
instance FromJSON NoteBody
instance ToJSON NoteBody

PTH.derivePersistField "NoteTitle"
PTH.derivePersistField "NoteBody"

makeBody :: Text -> Validation [VError] NoteBody
makeBody body
  | T.length body < 5 || T.length body > 300 = Failure [InvalidNoteBody]
  | otherwise = Success $ NoteBody body

makeTitle :: Text -> Validation [VError] NoteTitle
makeTitle name
  | nameLength < 5 || nameLength > 30 = Failure [InvalidName] 
  | otherwise = Success $ NoteTitle name
  where nameLength = T.length name

-- | Export for the swagger docs and tests
noteTitleSample :: NoteTitle
noteTitleSample = NoteTitle "some name"

noteBodySample :: NoteBody
noteBodySample = NoteBody "do something good"
