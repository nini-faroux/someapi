module NoteValidation (parseNote) where

import RIO
import RIO.Time (UTCTime, getCurrentTime, toGregorian, utctDay)
import Servant (errBody, err400)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import Model (NoteInput(..), Note(..))
import Validation (VError(..))
import App (App)
import NoteTypes (DayInput(..), makeBody, makeTitle, validDayText)
import qualified UserTypes

-- | Parses the note into a valid form
-- or returns the errors encountered
parseNote :: NoteInput -> App Note
parseNote noteInput = do
  time' <- liftIO getCurrentTime
  let (year, month, day') = toGregorian $ utctDay time'
  let day = DayInput year month day'
  case validNote noteInput time' day of
    Failure errs -> throwIO err400 { errBody = errorsToBS errs }
    Success note -> return note
  where
    errorsToBS :: [VError] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show ess

-- Produces either a valid note 
-- or accumulates any validation errors, encountered at each step, in a list
validNote :: NoteInput -> UTCTime -> DayInput -> Validation [VError] Note
validNote NoteInput {..} time day =
  Note 
    <$> UserTypes.makeName noteAuthor
    <*> makeTitle noteTitle
    <*> makeBody noteBody
    <*> validTime time
    <*> validDayText day

validTime :: UTCTime -> Validation [VError] UTCTime
validTime = Success
