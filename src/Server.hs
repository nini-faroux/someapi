{-# LANGUAGE TypeOperators #-}

module Server where

import Servant
import Control.Monad.Except
import RIO hiding (Handler)
import Api
import App

userServer :: ServerT UserAPI App
userServer =
       getUsers
  :<|> getUser
  :<|> createUser
  :<|> activateUserAccount
  :<|> loginUser
  :<|> getProtected
  :<|> getPrivate

hoistAppServer :: Env -> Server UserAPI
hoistAppServer env' = hoistServer userApi (transform env') userServer where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
