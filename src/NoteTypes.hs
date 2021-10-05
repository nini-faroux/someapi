{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module NoteTypes 
  ( NoteTitle
  , NoteBody
  , Day
  , DayField
  , Month
  , Year
  , NoteRequest(..)
  , DayInput(..)
  , validDay
  , makeTitle
  , makeBody
  , makeYear
  , makeMonth
  , makeDayField
  , noteTitleSample
  , noteBodySample
  , daySample
  ) where

import RIO (Text, Generic)
import Data.Validation (Validation(..))
import qualified Data.Text as T
import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import Validation (VError(..))

newtype NoteTitle = NoteTitle Text deriving (Eq, Show, Read, Generic)
newtype NoteBody = NoteBody Text deriving (Eq, Show, Read, Generic)

data Day = Day Year Month DayField deriving (Eq, Show, Read, Generic)

newtype Year = Year Integer deriving (Eq, Show, Read, Generic)
newtype Month = Month Int deriving (Eq, Show, Read, Generic)
newtype DayField = DayField Int deriving (Eq, Show, Read, Generic)

data NoteRequest = CreateNoteRequest | GetNoteRequest | GetNotesByNameRequest | GetNotesByDayRequest deriving Eq

data DayInput =
  DayInput {
    dayYear :: !Integer
  , dayMonth :: !Int
  , dayDay :: !Int
  } deriving (Eq, Show, Generic)

instance FromJSON NoteTitle
instance ToJSON NoteTitle
instance FromJSON NoteBody
instance ToJSON NoteBody
instance FromJSON Day
instance ToJSON Day
instance FromJSON Year
instance ToJSON Year
instance FromJSON Month
instance ToJSON Month
instance FromJSON DayField
instance ToJSON DayField
instance ToJSON DayInput
instance FromJSON DayInput

PTH.derivePersistField "NoteTitle"
PTH.derivePersistField "NoteBody"
PTH.derivePersistField "Day"

validDay :: DayInput -> Validation [VError] Day
validDay DayInput {..} = Day <$> makeYear dayYear <*> makeMonth dayMonth <*> makeDayField dayDay

makeBody :: Text -> Validation [VError] NoteBody
makeBody body
  | T.length body < 5 || T.length body > 300 = Failure [InvalidNoteBody]
  | otherwise = Success $ NoteBody body

makeTitle :: Text -> Validation [VError] NoteTitle
makeTitle name
  | nameLength < 5 || nameLength > 30 = Failure [InvalidName] 
  | otherwise = Success $ NoteTitle name
  where nameLength = T.length name

makeYear :: Integer -> Validation [VError] Year
makeYear year
  | year < 0 = Failure [InvalidYear]
  | otherwise = Success $ Year year

makeMonth :: Int -> Validation [VError] Month
makeMonth month
  | month < 0 || month > 12 = Failure [InvalidMonth]
  | otherwise = Success $ Month month

makeDayField :: Int -> Validation [VError] DayField
makeDayField day
  | day < 0 || day > 31 = Failure [InvalidDay]
  | otherwise = Success $ DayField day

-- | Export for the swagger docs and tests
noteTitleSample :: NoteTitle
noteTitleSample = NoteTitle "some name"

noteBodySample :: NoteBody
noteBodySample = NoteBody "do something good"

daySample :: Day
daySample = Day (Year 2021) (Month 10) (DayField 5)
