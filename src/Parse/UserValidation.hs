module Parse.UserValidation
  (parseUser
  ) where

import RIO
import Servant (errBody, err400)
import Data.Validation (Validation(..))
import qualified Data.ByteString.Lazy.UTF8 as LB
import Parse.Validation (VError(..), ThrowError(..))
import Parse.UserTypes (makeName, makeEmail, validActivation, validPassword)
import Web.Model (UserWithPassword(..), User(..))
import qualified Web.Query as Query
import Web.Query (Database)

parseUser :: ( Database env m
             , ThrowError m
             )
          => UserWithPassword
          -> m User
parseUser uwp@UserWithPassword {..} = do
  emailExists <- existsError email makeEmail ExistingEmail Query.getUserByEmail
  nameExists <- existsError name makeName ExistingUserName Query.getUserByName
  let emailExistsError = getError emailExists
      nameExistsError = getError nameExists
      passwordError = getError $ validPassword password
  case validUser uwp of
    Success user -> if null emailExistsError && null nameExistsError && null passwordError then return user
                    else throwError err400 { errBody = errorsToBS [emailExistsError, nameExistsError, passwordError] }
    Failure userErrors -> throwError err400 { errBody = errorsToBS [userErrors, emailExistsError, nameExistsError, passwordError] }
  where 
    existsError :: (Database env m)
                => Text
                -> (Text -> Validation [VError] a)
                -> VError
                -> (a -> m (Maybe b))
                -> m (Validation [VError] Bool)
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
    makeEmail email <*>
    validActivation (Just False)
