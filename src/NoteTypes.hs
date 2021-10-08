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
  , validDayText
  , makeValidDayText
  , makeValidDay
  , makeValidName
  , makeTitle
  , makeBody
  , makeYear
  , makeMonth
  , makeDayField
  , noteTitleSample
  , noteBodySample
  , daySample
  ) where

import Servant (errBody, err400)
import RIO (Text, Generic, isNothing, readMaybe, fromMaybe, throwIO)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import qualified Data.Text as T
import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import UserTypes (Name, makeName)
import Validation (VError(..))
import App (App)

newtype NoteTitle = NoteTitle Text deriving (Eq, Show, Read, Generic)
newtype NoteBody = NoteBody Text deriving (Eq, Show, Read, Generic)

data Day = Day Year Month DayField deriving (Eq, Ord, Read, Generic)

newtype Year = Year Integer deriving (Eq, Ord, Read, Generic)
newtype Month = Month Int deriving (Eq, Ord, Read, Generic)
newtype DayField = DayField Int deriving (Eq, Ord, Read, Generic)

instance Show Day where
  show (Day year month day) = show year ++ ":" ++ show month ++ ":" ++ show day

instance Show Year where
  show (Year year) = show year

instance Show Month where
  show (Month month)
    | month >= 0 && month < 10 = "0" ++ show month
    | otherwise = show month

instance Show DayField where
  show (DayField day)
    | day >= 0 && day < 10 = "0" ++ show day
    | otherwise = show day

data NoteRequest = CreateNoteRequest | GetNoteRequest deriving Eq

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

validDay' :: DayInput -> Validation [VError] Day
validDay' DayInput {..} = Day <$> makeYear dayYear <*> makeMonth dayMonth <*> makeDayField dayDay

makeValidDayText :: DayInput -> App Text
makeValidDayText date =
  case validDayText date of
    Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
    Success date' -> return date'

makeValidName :: Text -> App Name
makeValidName name =
  case makeName name of
    Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
    Success name' -> return name'

makeValidDay :: Text -> App Text
makeValidDay date =
  case validDay date of
    Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
    Success date' -> return date'

validDayText :: DayInput -> Validation [VError] Text
validDayText day =
  case validDay' day of
    Failure err -> Failure err
    Success vDay -> Success $ T.pack $ show vDay

validDay :: Text -> Validation [VError] Text
validDay dateParam
  | isNothing year || isNothing month || isNothing day = Failure [InvalidDate]
  | otherwise =
      case vDay of
        Failure err -> Failure err
        Success d -> Success $ T.pack $ show d
  where
    vDay = validDay' $ DayInput (fromMaybe 0 year) (fromMaybe 0 month) (fromMaybe 0 day)
    year = readMaybe $ take 4 dateParam' :: Maybe Integer
    month = readMaybe $ takeWhile isDigit $ drop 5 dateParam' :: Maybe Int
    day = readMaybe $ takeWhile isDigit $ drop 1 $ dropWhile isDigit $ drop 5 dateParam' :: Maybe Int
    dateParam' = T.unpack dateParam

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
  | year < 2020 = Failure [InvalidYear]
  | otherwise = Success $ Year year

makeMonth :: Int -> Validation [VError] Month
makeMonth month
  | month <= 0 || month > 12 = Failure [InvalidMonth]
  | otherwise = Success $ Month month

makeDayField :: Int -> Validation [VError] DayField
makeDayField day
  | day <= 0 || day > 31 = Failure [InvalidDay]
  | otherwise = Success $ DayField day

-- | Export for the swagger docs and tests
noteTitleSample :: NoteTitle
noteTitleSample = NoteTitle "some name"

noteBodySample :: NoteBody
noteBodySample = NoteBody "do something good"

daySample :: Text
daySample = T.pack $ show $ Day (Year 2021) (Month 10) (DayField 5)
