module Parse.Authenticate 
  ( makeAuthToken'
  , checkUserCredentials
  , CheckNameExists(..)
  , checkPassword'
  , getAuth
  , MakePassword(..)
  ) where

import RIO
import Servant (errBody, err400, err401, err404)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Password.Bcrypt (PasswordCheck(..), PasswordHash, Bcrypt, mkPassword, checkPassword, hashPassword)
import Database.Esqueleto.Experimental (Entity(..))
import Web.JWT (Token(..), Scope(..), MakeAuthToken(..), VerifyAuthToken(..))
import App (App, GetTime(..))
import Parse.UserTypes (Name)
import Parse.NoteTypes (MakeValidName(..))
import Web.Model (Auth(..))
import qualified Web.Query as Query
import Web.Query (Database)
import Parse.Validation (ThrowError(..))

makeAuthToken' :: ( Database env m
                  , GetTime m
                  , MakeAuthToken m
                  , ThrowError m
                  ) => Name -> m Token
makeAuthToken' existingName = do
  now <- getTime
  token <- makeAuthToken (Scope { protectedAccess = True, tokenUserName = existingName }) now
  case decodeUtf8' token of
    Left err -> throwError err400 { errBody = LB.fromString $ show err }
    Right token' -> return $ Token token'

checkUserCredentials :: ( CheckNameExists m
                        , MakeValidName m
                        , VerifyAuthToken m
                        ) 
                     => Maybe Token -> Text -> m (Name, Scope)
checkUserCredentials mToken author = do
  scope <- verifyAuthToken mToken
  name <- makeValidName author
  existingName <- checkNameExists name
  return (existingName, scope)

class Monad m => CheckNameExists m where
  checkNameExists :: Name -> m Name
instance CheckNameExists App where
  checkNameExists name = do
    mUser <- Query.getUserByName name
    case mUser of
      Nothing -> throwIO err404 { errBody = "User not found" }
      Just _user -> return name

class Monad m => MakePassword m where
  makePassword :: Text -> m (PasswordHash Bcrypt)
instance MakePassword App where
  makePassword txt = liftIO $ hashPassword $ mkPassword txt

checkPassword' :: ( Database env m
                  , ThrowError m)
                  => Text -> PasswordHash Bcrypt -> m PasswordCheck
checkPassword' loginPassword hashPass = do
  let pass = mkPassword loginPassword
  case checkPassword pass hashPass of
    PasswordCheckFail -> throwError err401 { errBody = authErrorMessage }
    PasswordCheckSuccess -> return PasswordCheckSuccess

getAuth :: ( Database env m
           , ThrowError m
           ) => Name -> m (PasswordHash Bcrypt)
getAuth name = do
  auth <- Query.getAuth name
  case auth of
    [Entity _ (Auth _uid hashPass)] -> return hashPass
    _err -> throwError err401 { errBody = authErrorMessage }

authErrorMessage :: LB.ByteString
authErrorMessage = "Incorrect username or password, or account not yet activated"
