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
  App,
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

-- | Conversion function from the RIO monad to Servant's ExceptT based monad
hoistAppServer :: Config -> Server NoteAPI
hoistAppServer config = hoistServer noteApi (transform config) noteServer
  where
    transform :: Config -> App a -> Handler a
    transform config' app = Handler $ ExceptT $ try $ runRIO config' app
