module Parse.Authenticate (
  WithPassword (..),
  WithName (..),
  checkUserCredentials,
  checkPassword',
  getAuth,
) where

import App (
  App,
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
import Parse.Validation (WithError (..))
import RIO
import Servant (
  err400,
  err401,
  err404,
  errBody,
 )
import Web.JWT (
  Scope,
  Token,
  WithAuthToken (..),
 )
import Web.Model (Auth (..), WithDatabase)
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

checkUserCredentials ::
  ( WithAuthToken m
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

class Monad m => WithPassword m where
  makePassword :: Text -> m (PasswordHash Bcrypt)

instance WithPassword App where
  makePassword txt = liftIO $ hashPassword $ mkPassword txt

checkPassword' ::
  ( WithDatabase env m
  , WithError m
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
  ( WithDatabase env m
  , WithError m
  ) =>
  Name ->
  m (PasswordHash Bcrypt)
getAuth name = do
  auth <- Query.getAuth name
  case auth of
    [Entity _ (Auth _uid hashPass)] -> return hashPass
    _err -> throwError err401 {errBody = authErrorMessage}

authErrorMessage :: LB.ByteString
authErrorMessage =
  "Incorrect username or password, or account not yet activated"
