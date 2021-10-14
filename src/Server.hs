module Server (hoistAppServer, noteServer) where

import Servant
import Control.Monad.Except (ExceptT(..))
import RIO hiding (Handler)
import Api (NoteAPI, noteApi, createUser, loginUser, activateUserAccount, getNotes, createNote, getNotesByName)
import App (App, Config)

noteServer :: ServerT NoteAPI App
noteServer =
       createUser
  :<|> activateUserAccount
  :<|> loginUser
  :<|> getNotes
  :<|> createNote
  :<|> getNotesByName

hoistAppServer :: Config -> Server NoteAPI
hoistAppServer env' = hoistServer noteApi (transform env') noteServer where
  transform :: Config -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
