module NoteValidation (parseNote) where

import RIO (throwIO, liftIO)
import RIO.Time (UTCTime, getCurrentTime)
import Servant (errBody, err400)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import Model (NoteInput(..), Note(..))
import Validation (VError(..))
import App (App)
import NoteTypes (makeBody, makeName)
import qualified UserTypes

parseNote :: NoteInput -> App Note
parseNote noteInput = do
  time' <- liftIO getCurrentTime
  case validNote noteInput time' of
    Success note -> return note
    Failure errs -> throwIO err400 { errBody = errorsToBS errs }
  where
    errorsToBS :: [VError] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show ess

validNote :: NoteInput -> UTCTime -> Validation [VError] Note
validNote NoteInput {..} time =
  Note <$> UserTypes.makeName noteAuthor <*> makeName noteTitle <*> makeBody noteBody <*> validTime time

validTime :: UTCTime -> Validation [VError] UTCTime
validTime = Success
