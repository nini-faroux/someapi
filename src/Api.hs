{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api 
  ( UserAPI
  , userApi
  , createUser
  , getUser
  , getUsers
  , loginUser
  , activateUserAccount
  , getProtected
  , getPrivate
  ) where

import Servant
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO (Text, Int64, encodeUtf8, decodeUtf8', throwIO, liftIO)
import RIO.Time (getCurrentTime)
import RIO.List (headMaybe)
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), InnerJoin(..), 
  select, from, on, table, val, where_, insert, fromSqlKey, val,
  (==.), (^.), (:&)(..))
import Data.Password.Bcrypt (PasswordCheck(..), mkPassword, checkPassword)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import App (App)
import Model
   (User(..), UserWithPassword(..), UserLogin(..), Auth(..), EntityField(..),
   Scope(..), ScopeField(..), Token(..), makePassword, runDB)
import Email (sendActivationLink)
import JWT (makeAuthToken, decodeAndValidateAuth, decodeAndValidateUser)
import Validation (parseUser)
import UserTypes (Email, makeEmail)

type UserAPI =
       GetUsers
  :<|> GetUser
  :<|> CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetProtected
  :<|> GetPrivate

type GetUser = "user" :> Capture "userid" (P.Key User) :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] Token
type GetProtected = "protected" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[JSON] Text
type GetPrivate = "private" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[JSON] Text

userApi :: Proxy UserAPI
userApi = Proxy

loginUser :: UserLogin -> App Token
loginUser UserLogin {..} = do
  email <- validEmail loginEmail
  exists <- emailExists email
  if not exists then throwIO err400 { errBody = "Email doesn't exist" }
  else do 
    auth <- getAuth email
    case auth of
      [Entity _ (Auth _uid hashPass)] -> do
        let pass' = mkPassword loginPassword
        case checkPassword pass' hashPass of
          PasswordCheckFail -> throwIO err401 { errBody = "Wrong password" }
          PasswordCheckSuccess -> do
            now <- getCurrentTime
            let token = makeAuthToken (Scope {protectedAccess = True, privateAccess = False}) now
            case decodeUtf8' token of
              Left _ -> return $ Token ""
              Right token' -> return $ Token token'
      _ -> throwIO err401 { errBody = "Authentication failed" }
    where
      validEmail email =
        case makeEmail email of
          Failure _err -> throwIO err400 { errBody = "Invalid email address" }
          Success email' -> return email'
      emailExists email = do
        mUser <- runDB $ P.selectFirst [UserEmail P.==. email] []
        case mUser of
          Nothing -> return False
          _user -> return True

getAuth :: Email -> App [Entity Auth]
getAuth email =
  runDB $
      select $ do
      (user :& auth) <-
          from $
          table @User `InnerJoin` table @Auth
          `on`
          (\(user' :& auth') -> user' ^. UserId ==. auth' ^. AuthUserId)
      where_ (user ^. UserEmail ==. val email)
      where_ (user ^. UserActivated ==. val (Just True))
      pure auth

createUser :: UserWithPassword -> App Int64
createUser uwp@UserWithPassword {..} = do
    user <- parseUser uwp
    pass <- liftIO $ makePassword password
    newUserId <- runDB $ insert user
    _ <- runDB . insert $ Auth {authUserId = newUserId, authPassword = pass}
    liftIO $ sendActivationLink user
    return $ fromSqlKey newUserId

getProtected :: Text -> Maybe Token -> App Text
getProtected = getProtectedResource Protected

getPrivate :: Text -> Maybe Token -> App Text
getPrivate = getProtectedResource Private

-- | Trim token before decoding to remove 'Bearer' prefix, otherwise invalid token error
getProtectedResource :: ScopeField -> Text -> Maybe Token -> App Text
getProtectedResource _ _ Nothing = throwIO err401 { errBody = "no token found" }
getProtectedResource scopeField _txt (Just (Token token)) = do
  eScope <- liftIO . decodeAndValidateAuth $ encodeUtf8 (T.init $ T.drop 8 token)
  case eScope of
    Left err -> throwIO err401 { errBody = LB.fromString err}
    Right Scope {..} -> case scopeField of
      Protected -> if protectedAccess then return (greet "protected") else throwIO err403 { errBody = "Not Authorised" }
      Private -> if privateAccess then return (greet "private") else throwIO err403 { errBody = "Not Authorized" }
    where greet resourceName = "access granted to " <> resourceName <> " resource"

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right user -> do
      _ <- runDB $ P.updateWhere [UserEmail P.==. user.userEmail,
                                  UserName P.==. user.userName,
                                  UserActivated P.==. Just False]
                                 [UserActivated P.=. Just True]
      runDB $ P.selectFirst [UserEmail P.==. user.userEmail] []

getFormInput :: MultipartData Mem -> Text
getFormInput formData = token
  where
    nameValuePairs = inputs formData
    token = maybe "" iValue $ headMaybe nameValuePairs

getUsers :: App [Entity User]
getUsers = runDB $ P.selectList [] []

getUser :: P.Key User -> App (Entity User)
getUser userId = do
  mUser <- runDB $ P.selectFirst [UserId P.==. userId] []
  case mUser of
    Nothing -> throwIO err404 { errBody = "User not found" }
    Just user -> return user
