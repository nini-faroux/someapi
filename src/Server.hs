module Server (hoistAppServer, userServer) where

import Servant
import Control.Monad.Except (ExceptT(..))
import RIO hiding (Handler)
import Api (UserAPI, userApi, getUsers, getUser, createUser, loginUser, activateUserAccount, getNotes, postNote)
import App (App, Env)

userServer :: ServerT UserAPI App
userServer =
       getUsers
  :<|> getUser
  :<|> createUser
  :<|> activateUserAccount
  :<|> loginUser
  :<|> getNotes
  :<|> postNote

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer userApi (transform env') userServer where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
