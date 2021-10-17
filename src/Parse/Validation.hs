module Parse.Validation
  (VError(..)
  ) where

import RIO

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
