module Query
  ( getAuth
  , insertAuth
  , getNotes
  , getNotesByName
  , getNotesByDay
  , insertNote
  , insertUser
  , getUsers
  , getUserById
  , getUserByEmail
  , getUserByName
  , updateUserActivatedValue
  ) where

import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), InnerJoin(..),
  select, from, on, table, val, where_, insert, val,
  (==.), (^.), (:&)(..))
import Model (User(..), Note(..), EntityField(..), Auth(..), runDB)
import Data.Password.Bcrypt (PasswordHash(..), Bcrypt)
import App (App)
import UserTypes (Name, Email)
import NoteTypes (Day)

getAuth :: Name -> App [Entity Auth]
getAuth name =
  runDB $
      select $ do
      (user :& auth) <-
          from $
          table @User `InnerJoin` table @Auth
          `on`
          (\(user' :& auth') -> user' ^. UserId ==. auth' ^. AuthUserId)
      where_ (user ^. UserName ==. val name)
      where_ (user ^. UserActivated ==. val (Just True))
      pure auth

getNotesByName :: Name -> App [Entity Note]
getNotesByName name = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteUserName ==. val name)
    pure note

getNotesByDay :: Day -> App [Entity Note]
getNotesByDay day = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteDayCreated ==. val day)
    pure note

insertAuth :: P.Key User -> PasswordHash Bcrypt -> App (P.Key Auth)
insertAuth userId password = runDB . insert $ Auth {authUserId = userId, authPassword = password}

getNotes :: App [Entity Note]
getNotes = runDB $ P.selectList [] []

insertNote :: Note -> App (P.Key Note)
insertNote note = runDB $ P.insert note

getUsers :: App [Entity User]
getUsers = runDB $ P.selectList [] []

insertUser :: User -> App (P.Key User)
insertUser user = runDB $ P.insert user

getUserById :: P.Key User -> App (Maybe (Entity User))
getUserById userId = runDB $ P.selectFirst [UserId P.==. userId] []

getUserByEmail :: Email -> App (Maybe (Entity User))
getUserByEmail email = runDB $ P.selectFirst [UserEmail P.==. email] []

getUserByName :: Name -> App (Maybe (Entity User))
getUserByName name = runDB $ P.selectFirst [UserName P.==. name] []

updateUserActivatedValue :: Email -> Name -> App ()
updateUserActivatedValue email name = runDB $ 
  P.updateWhere [ UserEmail P.==. email
                , UserName P.==. name
                , UserActivated P.==. Just False
                ]
                [UserActivated P.=. Just True]
