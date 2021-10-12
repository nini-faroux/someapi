module UserValidation (parseUser) where

import Servant (errBody, err400)
import RIO (Text, throwIO)
import Data.Validation (Validation(..))
import qualified Data.ByteString.Lazy.UTF8 as LB
import Validation (VError(..))
import Model (UserWithPassword(..), User(..))
import App (App)
import UserTypes (makeName, makeAge, makeEmail, validActivation, validPassword)
import qualified Query

parseUser :: UserWithPassword -> App User
parseUser uwp@UserWithPassword {..} = do
  emailExists <- existsError email makeEmail ExistingEmail Query.getUserByEmail
  nameExists <- existsError name makeName ExistingUserName Query.getUserByName
  let emailExistsError = getError emailExists
      nameExistsError = getError nameExists
      passwordError = getError $ validPassword password
  case validUser uwp of
    Success user -> if null emailExistsError && null nameExistsError && null passwordError then return user
                    else throwIO err400 { errBody = errorsToBS [emailExistsError, nameExistsError, passwordError] }
    Failure userErrors -> throwIO err400 { errBody = errorsToBS [userErrors, emailExistsError, nameExistsError, passwordError] }
  where 
    existsError :: Text
                -> (Text -> Validation [VError] a)
                -> VError
                -> (a -> App (Maybe b))
                -> App (Validation [VError] Bool)
    existsError txt make verror query =
      case make txt of
        Failure _err -> return $ Success False
        Success validEntry -> do
          mUser <- query validEntry
          case mUser of
            Nothing -> return $ Success False
            _user -> return $ Failure [verror]
    errorsToBS :: [[VError]] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show $ concat ess
    getError :: Validation [VError] a -> [VError]
    getError valid =
      case valid of
        Success _ -> []
        Failure err -> err

validUser :: UserWithPassword -> Validation [VError] User
validUser UserWithPassword {..} =
  User <$>
    makeName name <*>
    makeAge age <*>
    makeEmail email <*>
    validActivation (Just False)
