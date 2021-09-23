{-# LANGUAGE RecordWildCards #-}

module Validation where

import RIO
import qualified Data.Text as T
import Servant
import Data.Validation
import qualified Database.Persist as P
import qualified Text.Email.Validate as EV
import qualified Data.ByteString.Lazy.UTF8 as LB
import Model
import App

data VError =
    ExistingEmail
  | InvalidEmail
  | InvalidAge
  | InvalidName
  | InvalidPassword
  | InvalidActivation
  deriving Show

validEmail :: Text -> Validation [VError] Text
validEmail email
  | EV.isValid (encodeUtf8 email) = Success email
  | otherwise = Failure [InvalidEmail]

validAge :: Int -> Validation [VError] Int
validAge age
  | age >= 0 && age <= 120 = Success age
  | otherwise = Failure [InvalidAge]
  
validName :: Text -> Validation [VError] Text
validName name
  | nameLength < 4 || nameLength > 20 = Failure [InvalidName] 
  | otherwise = Success name
  where nameLength = T.length name

validPassword :: Text -> Validation [VError] Text
validPassword pass
  | lengthPass < 8 = Failure [InvalidPassword]
  | otherwise = Success pass
  where lengthPass = T.length pass

validActivation :: Maybe Bool -> Validation [VError] (Maybe Bool)
validActivation Nothing = Success Nothing
validActivation (Just activation)
  | activation = Failure [InvalidActivation]
  | otherwise = Success (Just False)

validUser :: UserWithPassword -> Validation [VError] User
validUser UserWithPassword {..} =
  User <$>
    validName name <*>
    validAge age <*>
    validEmail email <*>
    validActivation (Just False)

parseUser :: UserWithPassword -> App User
parseUser uwp@UserWithPassword {..} = do
  exists <- emailExists email
  let emailExists = otherError exists
  let passwordError = otherError $ validPassword password
  case validUser uwp of
    Success user -> if null emailExists && null passwordError then return user
                    else throwIO err400 { errBody = errorsToBS [emailExists, passwordError] }
    Failure userErrors -> throwIO err400 { errBody = errorsToBS [userErrors, emailExists, passwordError] }
  where 
    errorsToBS :: [[VError]] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show $ concat ess
    emailExists :: Text -> App (Validation [VError] Bool)
    emailExists email = do
      mUser <- runDB $ P.selectFirst [UserEmail P.==. email] []
      case mUser of
        Nothing -> return $ Success False
        _ -> return $ Failure [ExistingEmail]
    otherError :: Validation [VError] a -> [VError]
    otherError valid =
      case valid of
        Success _ -> []
        Failure err -> err
