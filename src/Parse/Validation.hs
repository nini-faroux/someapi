module Parse.Validation (
  Error (..),
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

class Monad m => Error m where
  throwError :: Exception e => e -> m a

instance Error App where
  throwError e = liftIO $ throwIO e

-- | For tests
instance Error IO where
  throwError = throwIO
