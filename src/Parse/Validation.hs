module Parse.Validation
  ( VError(..)
  , ThrowError(..)
  ) where

import RIO
import App (App)

data VError =
    ExistingEmail
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
  deriving Show

class Monad m => ThrowError m where
  throwError :: Exception e => e -> m a
instance ThrowError App where
  throwError e = liftIO $ throwIO e
