module Parse.Validation (
  WithError (..),
  VError (..),
) where

import App (App)
import RIO

data VError
  = ExistingEmail
  | ExistingUserName
  | InvalidEmail
  | InvalidAge
  | InvalidName
  | InvalidPassword
  | InvalidActivation
  | InvalidNoteBody
  | InvalidTime
  | InvalidDate
  | InvalidDay
  | InvalidYear
  | InvalidMonth
  | InvalidId
  deriving (Show)

class Monad m => WithError m where
  throwError :: Exception e => e -> m a

instance WithError App where
  throwError e = liftIO $ throwIO e

-- | For tests
instance WithError IO where
  throwError = throwIO
