module Server (hoistAppServer, noteServer) where

import Servant
import Control.Monad.Except (ExceptT(..))
import RIO hiding (Handler)
import Api (NoteAPI, noteApi, createUser, loginUser, activateUserAccount, getNotes, createNote, getNotesByName)
import App (App, Env)

noteServer :: ServerT NoteAPI App
noteServer =
       createUser
  :<|> activateUserAccount
  :<|> loginUser
  :<|> getNotes
  :<|> createNote
  :<|> getNotesByName

hoistAppServer :: Env -> Server NoteAPI
hoistAppServer env' = hoistServer noteApi (transform env') noteServer where
  transform :: Env -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
