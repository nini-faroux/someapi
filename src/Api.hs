{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
import App
import Model
import Email
import JWT

type UserAPI = GetUsers :<|> GetUser :<|> CreateUser :<|> ActivateUser :<|> LoginUser :<|> GetProtected :<|> GetPrivate

type GetUser = "user" :> ReqBody '[JSON] User :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Token
type GetProtected = "protected" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[PlainText] Text
type GetPrivate = "private" :> ReqBody '[JSON] Text :> Header "Authorization" Token :> Post '[PlainText] Text

userApi :: Proxy UserAPI
userApi = Proxy

userServer :: ServerT UserAPI App
userServer = getUsers :<|> getUser :<|> createUser :<|> activateUserAccount :<|> loginUser :<|> getProtected :<|> getPrivate

loginUser :: UserWithPassword -> App Token
loginUser userWP@UserWithPassword {..} = do
  auth <- getAuth userWP
  (Entity _ user) <- getUser $ User name Nothing email Nothing
  case auth of
    [Entity _ (Auth uid hashPass)] -> do
      let pass' = mkPassword password
      case checkPassword pass' hashPass of
        PasswordCheckFail -> throwIO err401 { errBody = "Wrong password" }
        PasswordCheckSuccess -> do
          now <- getCurrentTime
          let token = makeAuthToken (Scope {protectedAccess = True, privateAccess = False}) now
          case decodeUtf8' token of
            Left _ -> return $ Token ""
            Right token' -> return $ Token token'
    _ -> throwIO err401 { errBody = "Authentication failed" }

getAuth :: UserWithPassword -> App [Entity Auth]
getAuth UserWithPassword {..} =
  runDB $
      select $ do
      (user :& auth) <-
          from $
          table @User `InnerJoin` table @Auth
          `on`
          (\(user' :& auth') -> user' ^. UserId ==. auth' ^. AuthUserId)
      where_ (user ^. UserEmail ==. val email)
      where_ (user ^. UserName ==. val name)
      where_ (user ^. UserActivated ==. val (Just True))
      pure auth

createUser :: UserWithPassword -> App Int64
createUser UserWithPassword {..} = do
  let user = User name age email (Just False)
  exists <- emailExists user
  if exists then throwIO err403
  else do
    pass <- liftIO $ makePassword password
    newUser <- runDB $ insert user
    _ <- runDB . insert $ Auth {authUserId = newUser, authPassword = pass}
    liftIO $ sendActivationLink user
    return $ fromSqlKey newUser
  where
    emailExists User {..} = do
      mUser <- runDB $ P.selectFirst [UserEmail P.==. userEmail] []
      case mUser of
        Nothing -> return False
        _ -> return True

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
