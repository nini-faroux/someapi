{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Api where

import RIO hiding (Handler, (^.), on)
import RIO.Time
import RIO.List (headMaybe)
import Servant
import Servant.Server
import Servant.Multipart
import qualified Database.Persist as P
import Database.Esqueleto.Experimental 
  (Entity(..), InnerJoin(..), 
  select, from, on, table, val, where_, insert, fromSqlKey, val,
  (==.), (=.), (^.), (:&)(..))
import Data.Password.Bcrypt
import Control.Monad.Except
import App
import Model
import Email
import JWT

type UserAPI = GetUsers :<|> GetUser :<|> CreateUser :<|> ActivateUser :<|> LoginUser :<|> GetProtected

type GetUser = "user" :> ReqBody '[JSON] User :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Token
type GetProtected = "protected" :> ReqBody '[JSON] Token :> Post '[PlainText] Text

proxyAPI :: Proxy UserAPI
proxyAPI = Proxy

userSever :: ServerT UserAPI App
userSever = getUsers :<|> getUser :<|> createUser :<|> activateUserAccount :<|> loginUser :<|> getProtected

loginUser :: UserWithPassword -> App Token
loginUser userWP@UserWithPassword {..} = do
  auth <- getAuth userWP
  (Entity _ user) <- getUser $ User name Nothing email Nothing
  case auth of
    [Entity _ (Auth uid hashPass)] -> do
      let pass' = mkPassword password
      case checkPassword pass' hashPass of
        PasswordCheckFail -> throwIO err401
        PasswordCheckSuccess -> do
          now <- getCurrentTime
          let token = makeAuthToken (Scope {protected = True, private = False}) now
          case decodeUtf8' token of
            Left _ -> return $ Token "" ""
            Right token' -> return $ Token (user.userName) token'
    _ -> throwIO err401

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

getProtected :: Token -> App Text
getProtected (Token uName token) = do
  eScope <- liftIO . decodeAndValidateFullAuth $ encodeUtf8 token
  case eScope of
    Left err_ -> throwIO err401
    Right Scope {..} -> if protected then return ("hi " <> uName <> ", access granted") else throwIO err401

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateFull $ encodeUtf8 token
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

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer proxyAPI (transform env') userSever where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
