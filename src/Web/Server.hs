module Web.Server
  ( hoistAppServer
  , noteServer
  ) where

import RIO hiding (Handler)
import Servant
import Control.Monad.Except (ExceptT(..))
import Api (NoteAPI, noteApi, createUser, loginUser, activateUserAccount, getNotes, createNote, getNotesByName)
import App (App, Config)

-- | The Server for the API
noteServer :: ServerT NoteAPI App
noteServer =
       createUser
  :<|> activateUserAccount
  :<|> loginUser
  :<|> getNotes
  :<|> createNote
  :<|> getNotesByName

-- | Conversion function from the RIO monad to Servant's ExceptT based monad
hoistAppServer :: Config -> Server NoteAPI
hoistAppServer env' = hoistServer noteApi (transform env') noteServer where
  transform :: Config -> App a -> Handler a
  transform env app = Handler $ ExceptT $ try $ runRIO env app
