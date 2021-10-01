module NoteValidation (parseNote) where

-- import RIO (Text)
import RIO.Time (UTCTime)
-- import qualified Data.Text as T
-- import Data.Validation (Validation)
import Model (NoteInput(..), Note(..))
-- import UserTypes (makeName)
-- import Validation (VError(..))
import App (App)

parseNote :: NoteInput -> UTCTime -> App Note
parseNote _noteInput = undefined

-- validateNote :: NoteInput -> UTCTime -> Validation [VError] Note
-- validateNote NoteInput {..} time =
--   Note <$> validId userId <*> makeName noteName <*> validBody noteBody <*> validTime time

-- validId = undefined
-- validBody = undefined
-- validTime = undefined
