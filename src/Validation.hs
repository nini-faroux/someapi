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
import UserTypes

validUser :: UserWithPassword -> Validation [VError] User
validUser UserWithPassword {..} =
  User <$>
    makeName name <*>
    makeAge age <*>
    makeEmail email <*>
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
    emailExists email =
      case makeEmail email of
        Failure _err -> return $ Success False
        Success email' -> do
          mUser <- runDB $ P.selectFirst [UserEmail P.==. email'] []
          case mUser of
            Nothing -> return $ Success False
            _user -> return $ Failure [ExistingEmail]
    otherError :: Validation [VError] a -> [VError]
    otherError valid =
      case valid of
        Success _ -> []
        Failure err -> err
