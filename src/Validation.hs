module Validation (VError(..)) where

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
