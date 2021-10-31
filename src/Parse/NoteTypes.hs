{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Parse.NoteTypes 
  ( NoteTitle
  , NoteBody
  , Date
  , DateField
  , Month
  , Year
  , NoteRequest(..)
  , DateInput(..)
  , MakeValidDate(..)
  , MakeValidName(..)
  , makeDateInput
  , validateDate'
  , makeTitle
  , makeBody
  , makeYear
  , makeMonth
  , makeDayField
  , noteTitleSample
  , noteBodySample
  , dateSample
  ) where

import RIO
import Servant (errBody, err400)
import RIO.Time (toGregorian, utctDay)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import qualified Data.Text as T
import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isDigit)
import Parse.UserTypes (Name, makeName)
import Parse.Validation (VError(..))
import App (App, GetTime(..))

newtype NoteTitle = NoteTitle Text deriving (Eq, Show, Read, Generic)
newtype NoteBody = NoteBody Text deriving (Eq, Show, Read, Generic)

data Date = Date Year Month DateField deriving (Eq, Ord, Read, Generic)

newtype Year = Year Integer deriving (Eq, Ord, Read, Generic)
newtype Month = Month Int deriving (Eq, Ord, Read, Generic)
newtype DateField = DateField Int deriving (Eq, Ord, Read, Generic)

instance Show Date where
  show (Date year month date) = show year ++ ":" ++ show month ++ ":" ++ show date

instance Show Year where
  show (Year year) = show year

instance Show Month where
  show (Month month)
    | month >= 0 && month < 10 = "0" ++ show month
    | otherwise = show month

instance Show DateField where
  show (DateField date)
    | date >= 0 && date < 10 = "0" ++ show date
    | otherwise = show date

data NoteRequest = CreateNoteRequest | GetNoteRequest deriving Eq

data DateInput =
  DateInput {
    dateYear :: !Integer
  , dateMonth :: !Int
  , dateDay :: !Int
  } deriving (Eq, Show, Generic)

makeDateInput :: (GetTime m) => m DateInput
makeDateInput = do
  time <- getTime
  let (year, month, date) = toGregorian $ utctDay time
  return $ DateInput year month date

instance FromJSON NoteTitle
instance ToJSON NoteTitle
instance FromJSON NoteBody
instance ToJSON NoteBody
instance FromJSON Date
instance ToJSON Date
instance FromJSON Year
instance ToJSON Year
instance FromJSON Month
instance ToJSON Month
instance FromJSON DateField
instance ToJSON DateField
instance ToJSON DateInput
instance FromJSON DateInput

PTH.derivePersistField "NoteTitle"
PTH.derivePersistField "NoteBody"
PTH.derivePersistField "Date"

class Monad m => MakeValidName m where
  makeValidName :: Text -> m Name
instance MakeValidName App where
  makeValidName name =
    case makeName name of
      Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
      Success name' -> return name'

class Monad m => MakeValidDate m where
  makeValidDate :: Either Text DateInput -> m Text
instance MakeValidDate App where
  makeValidDate eDate =
    case eDate of
      Left dateText -> makeValidDate' validateDate dateText
      Right dateInput -> makeValidDate' validateDate' dateInput
    where
      makeValidDate' f date =
        case f date of
          Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
          Success vDate -> return vDate

validateDate' :: DateInput -> Validation [VError] Text
validateDate' DateInput {..} =
  let date = Date <$> makeYear dateYear <*> makeMonth dateMonth <*> makeDayField dateDay
   in case date of
     Failure err -> Failure err
     Success vDate -> Success $ T.pack $ show vDate

validateDate :: Text -> Validation [VError] Text
validateDate = validateDate' . makeDateInputFromText
  where
    makeDateInputFromText :: Text -> DateInput
    makeDateInputFromText dateParam = DateInput (fromMaybe 0 year) (fromMaybe 0 month) (fromMaybe 0 date)
      where
        year = readMaybe $ take 4 dateParam' :: Maybe Integer
        month = readMaybe $ takeWhile isDigit $ drop 5 dateParam' :: Maybe Int
        date = readMaybe $ takeWhile isDigit $ drop 1 $ dropWhile isDigit $ drop 5 dateParam' :: Maybe Int
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

makeDayField :: Int -> Validation [VError] DateField
makeDayField date
  | date <= 0 || date > 31 = Failure [InvalidDate]
  | otherwise = Success $ DateField date

-- | Export for the swagger docs and tests
noteTitleSample :: NoteTitle
noteTitleSample = NoteTitle "some name"

noteBodySample :: NoteBody
noteBodySample = NoteBody "do something good"

dateSample :: Text
dateSample = T.pack $ show $ Date (Year 2021) (Month 10) (DateField 5)
