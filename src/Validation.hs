module Validation (VError(..)) where

data VError =
    ExistingEmail
  | InvalidEmail
  | InvalidAge
  | InvalidName
  | InvalidPassword
  | InvalidActivation
  | InvalidNoteBody
  | InvalidTime
  | InvalidId
  deriving Show
