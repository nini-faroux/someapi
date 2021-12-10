module Parse.NoteValidation (
  parseNote,
) where

import App (
  WithTime (..),
 )
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation (..))
import qualified Database.Persist as P
import Parse.NoteTypes (
  DateInput (..),
  makeBody,
  makeDateInput,
  makeTitle,
  validateDate',
 )
import qualified Parse.UserTypes as UserTypes
import Parse.Validation (
  VError (..),
  WithError (..),
 )
import RIO
import RIO.Time (UTCTime)
import Servant (
  err400,
  errBody,
 )
import Web.Model (
  Note (..),
  NoteInput (..),
  User,
  WithDatabase,
 )
import qualified Web.Query as Query

{- | Parses the note into a valid form
 or returns the errors encountered
-}
parseNote ::
  ( WithDatabase env m
  , WithError m
  , WithTime m
  ) =>
  NoteInput ->
  m Note
parseNote noteInput@NoteInput {..} = do
  time' <- getTime
  name <- makeName' noteAuthor
  userId <- Query.getUserId name
  dateInput <- makeDateInput
  case validNote noteInput userId time' dateInput of
    Failure errs -> throwError err400 {errBody = errorsToBS errs}
    Success note -> return note
  where
    errorsToBS :: [VError] -> LB.ByteString
    errorsToBS = LB.fromString . show
    makeName' author =
      case UserTypes.makeName author of
        Failure errs -> throwError err400 {errBody = errorsToBS errs}
        Success name' -> pure name'

validNote ::
  NoteInput ->
  Maybe (P.Key User) ->
  UTCTime ->
  DateInput ->
  Validation [VError] Note
validNote NoteInput {..} userId time date =
  Note
    <$> validUserId userId
    <*> makeTitle noteTitle
    <*> makeBody noteBody
    <*> validTime time
    <*> validateDate' date

validUserId :: Maybe (P.Key User) -> Validation [VError] (P.Key User)
validUserId mId =
  case mId of
    Nothing -> Failure [InvalidId]
    Just id' -> Success id'

validTime :: UTCTime -> Validation [VError] UTCTime
validTime = Success
