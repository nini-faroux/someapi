{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api where

import RIO hiding ((^.), on)
import RIO.Time
import RIO.List (headMaybe)
import qualified Data.Text as T
import Servant
import Servant.Multipart
import qualified Database.Persist as P
import Database.Esqueleto.Experimental 
  (Entity(..), InnerJoin(..), 
  select, from, on, table, val, where_, insert, fromSqlKey, val,
  (==.), (=.), (^.), (:&)(..))
import Data.Password.Bcrypt
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Text.Email.Validate as EV
import App
import Model
import Email
import JWT
import Validation

type UserAPI =
       GetUsers
  :<|> GetUser
  :<|> CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetProtected
  :<|> GetPrivate

type GetUser = "user" :> ReqBody '[JSON] User :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] Token
type GetProtected = "protected" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[PlainText] Text
type GetPrivate = "private" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[PlainText] Text

userApi :: Proxy UserAPI
userApi = Proxy

loginUser :: UserLogin -> App Token
loginUser userWP@UserLogin {..} = do
  exists <- emailExists loginEmail
  if not exists then throwIO err400 { errBody = "Email doesn't exist" }
  else do 
    auth <- getAuth userWP
    case auth of
      [Entity _ (Auth uid hashPass)] -> do
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
      emailExists email = do
        mUser <- runDB $ P.selectFirst [UserEmail P.==. email] []
        case mUser of
          Nothing -> return False
          _user -> return True

getAuth :: UserLogin -> App [Entity Auth]
getAuth UserLogin {..} =
  runDB $
      select $ do
      (user :& auth) <-
          from $
          table @User `InnerJoin` table @Auth
          `on`
          (\(user' :& auth') -> user' ^. UserId ==. auth' ^. AuthUserId)
      where_ (user ^. UserEmail ==. val loginEmail)
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
getProtectedResource scopeField txt (Just (Token token)) = do
  eScope <- liftIO . decodeAndValidateAuth $ encodeUtf8 (T.init $ T.drop 8 token)
  case eScope of
    Left err -> throwIO err401 { errBody = LB.fromString err}
    Right Scope {..} -> case scopeField of
      Protected -> if protectedAccess then return (greet "protected") else throwIO err403
      Private -> if privateAccess then return (greet "private") else throwIO err403
    where greet resourceName = "access granted to " <> resourceName <> " resource"

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err404
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

getUser :: User -> App (Entity User)
getUser User {..} = do
  mUser <- runDB $ P.selectFirst [UserName P.==. userName, UserEmail P.==. userEmail] []
  case mUser of
    Nothing -> throwIO err404
    Just user -> return user
