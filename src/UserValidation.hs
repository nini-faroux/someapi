module UserValidation (parseUser) where

import Servant (errBody, err400)
import RIO (Text, throwIO)
import Data.Validation (Validation(..))
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Database.Persist as P
import Validation (VError(..))
import Model (UserWithPassword(..), User(..), EntityField(UserEmail), runDB)
import App (App)
import UserTypes (makeName, makeAge, makeEmail, validActivation, validPassword)

parseUser :: UserWithPassword -> App User
parseUser uwp@UserWithPassword {..} = do
  exists <- emailExists email
  let emailExistsError = otherError exists
  let passwordError = otherError $ validPassword password
  case validUser uwp of
    Success user -> if null emailExistsError && null passwordError then return user
                    else throwIO err400 { errBody = errorsToBS [emailExistsError, passwordError] }
    Failure userErrors -> throwIO err400 { errBody = errorsToBS [userErrors, emailExistsError, passwordError] }
  where 
    errorsToBS :: [[VError]] -> LB.ByteString
    errorsToBS ess = LB.fromString $ show $ concat ess
    emailExists :: Text -> App (Validation [VError] Bool)
    emailExists emailAddr =
      case makeEmail emailAddr of
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

validUser :: UserWithPassword -> Validation [VError] User
validUser UserWithPassword {..} =
  User <$>
    makeName name <*>
    makeAge age <*>
    makeEmail email <*>
    validActivation (Just False)
