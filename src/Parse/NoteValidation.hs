module Parse.NoteValidation 
  (parseNote
  ) where

import RIO
import RIO.Time (UTCTime)
import Servant (errBody, err400)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import Web.Model (NoteInput(..), Note(..))
import Parse.Validation (VError(..), ThrowError(..))
import App (GetTime(..))
import Parse.NoteTypes (DateInput(..), makeDateInput, makeBody, makeTitle, validDateInput)
import qualified Parse.UserTypes as UserTypes

-- | Parses the note into a valid form
-- or returns the errors encountered
parseNote :: ( GetTime m
             , ThrowError m) => NoteInput -> m Note
parseNote noteInput = do
  time' <- getTime
  dayInput <- makeDateInput
  case validNote noteInput time' dayInput of
    Failure errs -> throwError err400 { errBody = errorsToBS errs }
    Success note -> return note
  where
    errorsToBS :: [VError] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show ess

-- Produces either a valid note 
-- or accumulates any validation errors, encountered at each step, in a list
validNote :: NoteInput -> UTCTime -> DateInput -> Validation [VError] Note
validNote NoteInput {..} time day =
  Note 
    <$> UserTypes.makeName noteAuthor
    <*> makeTitle noteTitle
    <*> makeBody noteBody
    <*> validTime time
    <*> validDateInput day

validTime :: UTCTime -> Validation [VError] UTCTime
validTime = Success
