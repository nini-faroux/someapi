{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import RIO hiding (Handler)
import Servant
import Servant.Server
import Servant.Multipart
import Database.Persist.Postgresql (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.), (=.), updateWhere)
import Control.Monad.Except
import App
import Model
import Email

type UserAPI =
       "users" :> Get '[JSON] [Entity User]
  :<|> "user" :> Capture "name" Text :> Get '[JSON] (Entity User)
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] NoContent

proxyAPI :: Proxy UserAPI
proxyAPI = Proxy

userSever :: ServerT UserAPI App
userSever = getUsers :<|> getUser :<|> createUser :<|> activateUserAccount

activateUserAccount :: MultipartData Mem -> App NoContent
activateUserAccount formData = do
  let [name, email] = getFormInput formData
  _ <- runDB $ updateWhere [UserEmail ==. email, UserName ==. name, UserActivated ==. Just False] [UserActivated =. Just True]
  return NoContent

getFormInput :: MultipartData Mem -> [Text]
getFormInput formData = [name, email]
  where
    nameValuePairs = inputs formData
    name = iValue $ head nameValuePairs
    email = iValue $ last nameValuePairs

getUsers :: App [Entity User]
getUsers = runDB (selectList [] [])

getUser :: Text -> App (Entity User)
getUser name = do
  mUser <- runDB (selectFirst [UserName ==. name] [])
  case mUser of
    Nothing -> throwIO err404
    Just user -> return user

createUser :: User -> App Int64
createUser user = do
  newUser <- runDB $ insert $ User (userName user) (userAge user) (userEmail user) (Just False)
  liftIO $ sendActivationLink user
  return $ fromSqlKey newUser

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer proxyAPI (transform env') userSever where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
