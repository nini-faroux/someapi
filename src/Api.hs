{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Api where

import RIO hiding (Handler)
import RIO.Time
import RIO.List (headMaybe)
import Servant
import Servant.Server
import Servant.Multipart
import Database.Persist.Postgresql 
  (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.), (=.), updateWhere)
import Control.Monad.Except
import App
import Model
import Email
import JWT

type UserAPI = GetUsers :<|> GetUser :<|> CreateUser :<|> ActivateUser

type GetUser = "user" :> Capture "name" Text :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))

proxyAPI :: Proxy UserAPI
proxyAPI = Proxy

userSever :: ServerT UserAPI App
userSever = getUsers :<|> getUser :<|> createUser :<|> activateUserAccount

createUser :: UserWithPassword -> App Int64
createUser UserWithPassword {..} = do
  let user = User { userName = name, userAge = age, userEmail = email, userActivated = Just False }
  pass <- liftIO $ makePassword password
  newUser <- runDB $ insert user
  _ <- runDB . insert $ Auth {authUserId = newUser, authPassword = pass}
  liftIO $ sendActivationLink user
  return $ fromSqlKey newUser

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateFull $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err404
    Right user -> do
      _ <- runDB $ updateWhere [UserEmail ==. userEmail user, UserName ==. userName user, UserActivated ==. Just False] [UserActivated =. Just True]
      runDB $ selectFirst [UserEmail ==. userEmail user] []

getFormInput :: MultipartData Mem -> Text
getFormInput formData = token
  where
    nameValuePairs = inputs formData
    token = maybe "" iValue $ headMaybe nameValuePairs

getUsers :: App [Entity User]
getUsers = runDB (selectList [] [])

getUser :: Text -> App (Entity User)
getUser name = do
  mUser <- runDB (selectFirst [UserName ==. name] [])
  case mUser of
    Nothing -> throwIO err404
    Just user -> return user

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer proxyAPI (transform env') userSever where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
