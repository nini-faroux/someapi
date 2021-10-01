{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api 
  ( UserAPI
  , CreateUser
  , GetUser
  , GetUsers
  , userApi
  , createUser
  , getUser
  , getUsers
  , loginUser
  , activateUserAccount
  , getProtected
  , getAdmin
  ) where

import Servant
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO (Text, Int64, encodeUtf8, decodeUtf8', throwIO, liftIO)
import RIO.Time (getCurrentTime)
import RIO.List (headMaybe)
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), fromSqlKey)
import Data.Password.Bcrypt (PasswordCheck(..), mkPassword, checkPassword)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import App (App)
import Model
   (User(..), UserWithPassword(..), UserLogin(..), Auth(..),
   Scope(..), ScopeField(..), Token(..), makePassword)
import Email (sendActivationLink)
import JWT (makeAuthToken, decodeAndValidateAuth, decodeAndValidateUser)
import Validation (parseUser)
import UserTypes (makeEmail)
import qualified Query

type UserAPI =
       GetUsers
  :<|> GetUser
  :<|> CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetProtected
  :<|> GetAdmin

type GetUser = "user" :> Capture "userid" (P.Key User) :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] Token
type GetProtected = "protected" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[JSON] Text
type GetAdmin = "admin" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[JSON] Text

userApi :: Proxy UserAPI
userApi = Proxy

loginUser :: UserLogin -> App Token
loginUser UserLogin {..} = do
  email <- validEmail loginEmail
  exists <- emailExists email
  if not exists then throwIO err401 { errBody = authErrorMessage }
  else do 
    auth <- Query.getAuth email
    case auth of
      [Entity _ (Auth _uid hashPass)] -> do
        let pass' = mkPassword loginPassword
        case checkPassword pass' hashPass of
          PasswordCheckFail -> throwIO err401 { errBody = authErrorMessage }
          PasswordCheckSuccess -> do
            now <- getCurrentTime
            let token = makeAuthToken (Scope {protectedAccess = True, adminAccess = False}) now
            case decodeUtf8' token of
              Left _ -> return $ Token ""
              Right token' -> return $ Token token'
      _ -> throwIO err401 { errBody = authErrorMessage }
    where
      validEmail email =
        case makeEmail email of
          Failure _err -> throwIO err400 { errBody = "Invalid email address" }
          Success email' -> return email'
      emailExists email = do
        mUser <- Query.getUserByEmail email
        case mUser of
          Nothing -> return False
          _user -> return True
      authErrorMessage = "Incorrect username or password, or account not yet activated"

createUser :: UserWithPassword -> App Int64
createUser uwp@UserWithPassword {..} = do
    user <- parseUser uwp
    pass <- liftIO $ makePassword password
    newUserId <- Query.insertUser user
    _ <- Query.insertAuth newUserId pass
    liftIO $ sendActivationLink user
    return $ fromSqlKey newUserId

getUsers :: App [Entity User]
getUsers = Query.getUsers

getUser :: P.Key User -> App (Entity User)
getUser userId = do
  mUser <- Query.getUserById userId
  case mUser of
    Nothing -> throwIO err404 { errBody = "User not found" }
    Just user -> return user

getProtected :: Text -> Maybe Token -> App Text
getProtected = getProtectedResource Protected

getAdmin :: Text -> Maybe Token -> App Text
getAdmin = getProtectedResource Admin

-- | Trim token before decoding to remove 'Bearer' prefix, otherwise invalid token error
getProtectedResource :: ScopeField -> Text -> Maybe Token -> App Text
getProtectedResource _ _ Nothing = throwIO err401 { errBody = "No token found" }
getProtectedResource scopeField _txt (Just (Token token)) = do
  eScope <- liftIO . decodeAndValidateAuth $ encodeUtf8 (T.init $ T.drop 8 token)
  case eScope of
    Left err -> throwIO err400 { errBody = LB.fromString err}
    Right Scope {..} -> case scopeField of
      Protected -> if protectedAccess then return (greet "protected") else throwIO err403 { errBody = "Not Authorised" }
      Admin -> if adminAccess then return (greet "admin") else throwIO err403 { errBody = "Not Authorized" }
    where greet resourceName = "access granted to " <> resourceName <> " resource"

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right user -> do
      Query.updateUserActivatedValue user.userEmail user.userName
      Query.getUserByEmail user.userEmail

getFormInput :: MultipartData Mem -> Text
getFormInput formData = token
  where
    nameValuePairs = inputs formData
    token = maybe "" iValue $ headMaybe nameValuePairs
