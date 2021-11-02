module Parse.Authenticate (
  Password (..),
  WithName (..),
  checkUserCredentials,
  checkPassword',
  getAuth,
  makeAuthToken',
) where

import App (
  App,
  WithTime (..),
 )
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Password.Bcrypt (
  Bcrypt,
  PasswordCheck (..),
  PasswordHash,
  checkPassword,
  hashPassword,
  mkPassword,
 )
import Data.Validation (Validation (..))
import Database.Esqueleto.Experimental (Entity (..))
import Parse.UserTypes (Name, makeName)
import Parse.Validation (Error (..))
import RIO
import Servant (
  err400,
  err401,
  err404,
  errBody,
 )
import Web.JWT (
  AuthToken (..),
  Scope (..),
  Token (..),
 )
import Web.Model (Auth (..))
import Web.Query (Database)
import qualified Web.Query as Query

class Monad m => WithName m where
  makeValidName :: Text -> m Name
  checkNameExists :: Name -> m Name

instance WithName App where
  makeValidName name =
    case makeName name of
      Failure err -> throwError err400 {errBody = LB.fromString $ show err}
      Success name' -> return name'

  checkNameExists name = do
    mUser <- Query.getUserByName name
    case mUser of
      Nothing -> throwError err404 {errBody = "User not found"}
      Just _user -> return name

makeAuthToken' ::
  ( AuthToken m
  , Error m
  , WithTime m
  ) =>
  Name ->
  m Token
makeAuthToken' existingName = do
  now <- getTime
  token <- makeAuthToken (Scope {protectedAccess = True, tokenUserName = existingName}) now
  case decodeUtf8' token of
    Left err -> throwError err400 {errBody = LB.fromString $ show err}
    Right token' -> return $ Token token'

checkUserCredentials ::
  ( AuthToken m
  , WithName m
  ) =>
  Maybe Token ->
  Text ->
  m (Name, Scope)
checkUserCredentials mToken author = do
  scope <- verifyAuthToken mToken
  name <- makeValidName author
  existingName <- checkNameExists name
  return (existingName, scope)

class Monad m => Password m where
  makePassword :: Text -> m (PasswordHash Bcrypt)

instance Password App where
  makePassword txt = liftIO $ hashPassword $ mkPassword txt

checkPassword' ::
  ( Database env m
  , Error m
  ) =>
  Text ->
  PasswordHash Bcrypt ->
  m PasswordCheck
checkPassword' loginPassword hashPass = do
  let pass = mkPassword loginPassword
  case checkPassword pass hashPass of
    PasswordCheckFail -> throwError err401 {errBody = authErrorMessage}
    PasswordCheckSuccess -> return PasswordCheckSuccess

getAuth ::
  ( Database env m
  , Error m
  ) =>
  Name ->
  m (PasswordHash Bcrypt)
getAuth name = do
  auth <- Query.getAuth name
  case auth of
    [Entity _ (Auth _uid hashPass)] -> return hashPass
    _err -> throwError err401 {errBody = authErrorMessage}

authErrorMessage :: LB.ByteString
authErrorMessage = "Incorrect username or password, or account not yet activated"
