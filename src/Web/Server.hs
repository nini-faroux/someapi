module Web.Server
  ( hoistAppServer
  , noteServer
  ) where

import RIO hiding (Handler)
import Servant
import Control.Monad.Except (ExceptT(..))
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

-- | Conversion function from the RIO monad to Servant's ExceptT based monad
hoistAppServer :: Config -> Server NoteAPI
hoistAppServer config = hoistServer noteApi (transform config) noteServer where
  transform :: Config -> App a -> Handler a
  transform config' app = Handler $ ExceptT $ try $ runRIO config' app
