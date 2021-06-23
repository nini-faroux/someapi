{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import RIO hiding (Handler)
import Servant
import Servant.Server
import Database.Persist.Postgresql (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.))
import Control.Monad.Except
import App
import Model

type UserAPI =
       "users" :> Get '[JSON] [Entity User]
  :<|> "user" :> Capture "name" Text :> Get '[JSON] (Entity User)
  :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] Int64

proxyAPI :: Proxy UserAPI
proxyAPI = Proxy

userSever :: ServerT UserAPI App
userSever = getUsers :<|> getUser :<|> createUser

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
  newUser <- runDB (insert (User (userName user) (userAge user) (userEmail user)))
  return $ fromSqlKey newUser

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer proxyAPI (transform env') userSever where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
