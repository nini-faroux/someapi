module Web.Server (
  hoistAppServer,
  noteServer,
) where

import Api (
  NoteAPI,
  activateUserAccount,
  createNote,
  createUser,
  getNotes,
  getNotesByName,
  loginUser,
  noteApi,
 )
import App (
  App (unApp),
  Config,
 )
import Control.Monad.Except (ExceptT (..))
import RIO hiding (Handler)
import Servant

noteServer :: ServerT NoteAPI App
noteServer =
  createUser
    :<|> activateUserAccount
    :<|> loginUser
    :<|> getNotes
    :<|> createNote
    :<|> getNotesByName

-- | Conversion function from the ReaderT monad to Servant's ExceptT based monad
hoistAppServer :: Config App -> Server NoteAPI
hoistAppServer config = hoistServer noteApi (transform config) noteServer
  where
    transform :: Config App -> App a -> Handler a
    transform config' app =
      Handler $ ExceptT $ try $ runReaderT (unApp app) config'
