module Web.Query
  ( getAuth
  , insertAuth
  , getNotes
  , getNotesByName
  , getNotesByDate
  , getNotesBetweenDates
  , getNotesBetweenDatesWithName
  , getFirstDate
  , insertNote
  , insertUser
  , getUsers
  , getUserById
  , getUserByEmail
  , getUserByName
  , updateUserActivatedValue
  , Database
  ) where

import RIO hiding ((^.), on)
import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), InnerJoin(..),
  select, from, on, table, val, where_, insert, val, min_, unValue,
  (==.), (>=.), (<=.), (^.), (:&)(..))
import Data.Password.Bcrypt (PasswordHash(..), Bcrypt)
import Web.Model (User(..), Note(..), EntityField(..), Auth(..), Database, runDB)
import Parse.UserTypes (Name, Email)

getAuth :: Database env m => Name -> m [Entity Auth]
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

getNotesBetweenDates :: Database env m => Text -> Text -> m [Entity Note]
getNotesBetweenDates start end = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteDateCreated >=. val start)
    where_ (note ^. NoteDateCreated  <=. val end)
    pure note

getNotesBetweenDatesWithName :: Database env m => Name -> Text -> Text -> m [Entity Note]
getNotesBetweenDatesWithName name start end = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteUserName ==. val name)
    where_ (note ^. NoteDateCreated >=. val start)
    where_ (note ^. NoteDateCreated <=. val end)
    pure note

getNotesByName :: Database env m => Name -> m [Entity Note]
getNotesByName name = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteUserName ==. val name)
    pure note

getFirstDate :: Database env m => m (Maybe Text)
getFirstDate = runDB $ do
  days <- select $ do
    note <- from $ table @Note
    let mDate = min_ (note ^. NoteDateCreated)
    pure mDate
  case days of
    [] -> pure Nothing
    (day:_) -> pure $ unValue day

getNotesByDate :: Database env m => Text -> m [Entity Note]
getNotesByDate day = runDB $
  select $ do
    note <- from $ table @Note
    where_ (note ^. NoteDateCreated ==. val day)
    pure note

insertAuth :: Database env m => P.Key User -> PasswordHash Bcrypt -> m (P.Key Auth)
insertAuth userId password = runDB . insert $ Auth {authUserId = userId, authPassword = password}

getNotes :: Database env m => m [Entity Note]
getNotes = runDB $ P.selectList [] []

insertNote :: Database env m => Note -> m (P.Key Note)
insertNote note = runDB $ P.insert note

getUsers :: Database env m => m [Entity User]
getUsers = runDB $ P.selectList [] []

insertUser :: Database env m => User -> m (P.Key User)
insertUser user = runDB $ P.insert user

getUserById :: Database env m => P.Key User -> m (Maybe (Entity User))
getUserById userId = runDB $ P.selectFirst [UserId P.==. userId] []

getUserByEmail :: Database env m => Email -> m (Maybe (Entity User))
getUserByEmail email = runDB $ P.selectFirst [UserEmail P.==. email] []

getUserByName :: Database env m => Name -> m (Maybe (Entity User))
getUserByName name = runDB $ P.selectFirst [UserName P.==. name] []

updateUserActivatedValue :: Database env m => Email -> Name -> m ()
updateUserActivatedValue email name = runDB $ 
  P.updateWhere [ UserEmail P.==. email
                , UserName P.==. name
                , UserActivated P.==. Just False
                ]
                [UserActivated P.=. Just True]
