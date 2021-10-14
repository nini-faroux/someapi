module Authenticate 
  ( makeAuthToken'
  , checkUserCredentials
  , checkNameExists
  , makePassword
  , checkPassword'
  , getAuth
  ) where

import Servant (errBody, err400, err401, err404)
import RIO (Text, throwIO, decodeUtf8', liftIO)
import RIO.Time (getCurrentTime)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Password.Bcrypt (PasswordCheck(..), PasswordHash, Bcrypt, mkPassword, checkPassword, hashPassword)
import Database.Esqueleto.Experimental (Entity(..))
import JWT (Token(..), Scope(..), makeAuthToken, verifyAuthToken)
import App (App)
import UserTypes (Name)
import NoteTypes (makeValidName)
import Model (Auth(..))
import qualified Query

makeAuthToken' :: Name -> App Token
makeAuthToken' existingName = do
  now <- liftIO getCurrentTime
  let token = makeAuthToken (Scope { protectedAccess = True, tokenUserName = existingName }) now
  case decodeUtf8' token of
    Left err -> throwIO err400 { errBody = LB.fromString $ show err }
    Right token' -> return $ Token token'

checkUserCredentials :: Maybe Token -> Text -> App (Name, Scope)
checkUserCredentials mToken author = do
  scope <- verifyAuthToken mToken
  name <- makeValidName author
  existingName <- checkNameExists name
  return (existingName, scope)

checkNameExists :: Name -> App Name
checkNameExists name = do
  mUser <- Query.getUserByName name
  case mUser of
    Nothing -> throwIO err404 { errBody = "User not found" }
    Just _user -> return name

makePassword :: Text -> IO (PasswordHash Bcrypt)
makePassword = hashPassword . mkPassword

checkPassword' :: Text -> PasswordHash Bcrypt -> App PasswordCheck
checkPassword' loginPassword hashPass = do
  let pass = mkPassword loginPassword
  case checkPassword pass hashPass of
    PasswordCheckFail -> throwIO err401 { errBody = authErrorMessage }
    PasswordCheckSuccess -> return PasswordCheckSuccess

getAuth :: Name -> App (PasswordHash Bcrypt)
getAuth name = do
  auth <- Query.getAuth name
  case auth of
    [Entity _ (Auth _uid hashPass)] -> return hashPass
    _err -> throwIO err401 { errBody = authErrorMessage }

authErrorMessage :: LB.ByteString
authErrorMessage = "Incorrect username or password, or account not yet activated"
