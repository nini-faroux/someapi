module NoteValidation (parseNote) where

import RIO (Text, throwIO, liftIO)
import RIO.Time (UTCTime, getCurrentTime)
import Servant (errBody, err400)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import Model (NoteInput(..), Note(..), User(..), Key)
import UserTypes (makeName)
import Validation (VError(..))
import App (App)

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
  Note <$> validId userId <*> makeName noteName <*> validBody noteBody <*> validTime time

validId :: Key User -> Validation [VError] (Key User)
validId = Success

validBody :: Text -> Validation [VError] T.Text
validBody body
  | T.length body < 5 || T.length body > 300 = Failure [InvalidNoteBody]
  | otherwise = Success body

validTime :: UTCTime -> Validation [VError] UTCTime
validTime = Success
