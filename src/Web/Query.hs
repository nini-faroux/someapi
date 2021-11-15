module Web.Query (
  WithDatabase,
  getAuth,
  getFirstDate,
  getNotes,
  getNotesBetweenDates,
  getNotesBetweenDatesWithName,
  getNotesByDate,
  getNotesByName,
  getUserByEmail,
  getUserById,
  getUserByName,
  getUsers,
  insertAuth,
  insertNote,
  insertUser,
  updateUserActivatedValue,
) where

import Data.Password.Bcrypt (Bcrypt, PasswordHash (..))
import Database.Esqueleto.Experimental (
  Entity (..),
  InnerJoin (..),
  from,
  insert,
  min_,
  on,
  select,
  table,
  unValue,
  val,
  where_,
  (:&) (..),
  (<=.),
  (==.),
  (>=.),
  (^.),
 )
import qualified Database.Persist as P
import Parse.UserTypes (
  Email,
  Name,
 )
import RIO hiding (
  on,
  (^.),
 )
import Web.Model (
  Auth (..),
  EntityField (..),
  Note (..),
  User (..),
  WithDatabase,
  runDB,
 )

getAuth :: WithDatabase env m => Name -> m [Entity Auth]
getAuth name =
  runDB $
    select $
      do
        (user :& auth) <-
          from $
            table @User `InnerJoin` table @Auth
              `on` ( \(user' :& auth') ->
                      user' ^. UserId ==. auth' ^. AuthUserId
                   )
        where_ (user ^. UserName ==. val name)
        where_ (user ^. UserActivated ==. val (Just True))
        pure auth

getNotesBetweenDates ::
  WithDatabase env m =>
  Text ->
  Text ->
  m [Entity Note]
getNotesBetweenDates start end = runDB $
  select $
    do
      note <- from $ table @Note
      where_ (note ^. NoteDateCreated >=. val start)
      where_ (note ^. NoteDateCreated <=. val end)
      pure note

getNotesBetweenDatesWithName ::
  WithDatabase env m =>
  Name ->
  Text ->
  Text ->
  m [Entity Note]
getNotesBetweenDatesWithName name start end = runDB $
  select $
    do
      note <- from $ table @Note
      where_ (note ^. NoteUserName ==. val name)
      where_ (note ^. NoteDateCreated >=. val start)
      where_ (note ^. NoteDateCreated <=. val end)
      pure note

getNotesByName :: WithDatabase env m => Name -> m [Entity Note]
getNotesByName name = runDB $
  select $
    do
      note <- from $ table @Note
      where_ (note ^. NoteUserName ==. val name)
      pure note

getFirstDate :: WithDatabase env m => m (Maybe Text)
getFirstDate = runDB $ do
  days <- select $ do
    note <- from $ table @Note
    let mDate = min_ (note ^. NoteDateCreated)
    pure mDate
  case days of
    [] -> pure Nothing
    (day : _) -> pure $ unValue day

getNotesByDate :: WithDatabase env m => Text -> m [Entity Note]
getNotesByDate day = runDB $
  select $
    do
      note <- from $ table @Note
      where_ (note ^. NoteDateCreated ==. val day)
      pure note

insertAuth ::
  WithDatabase env m =>
  P.Key User ->
  PasswordHash Bcrypt ->
  m (P.Key Auth)
insertAuth userId password =
  runDB . insert $ Auth {authUserId = userId, authPassword = password}

getNotes :: WithDatabase env m => m [Entity Note]
getNotes = runDB $ P.selectList [] []

insertNote :: WithDatabase env m => Note -> m (P.Key Note)
insertNote note = runDB $ P.insert note

getUsers :: WithDatabase env m => m [Entity User]
getUsers = runDB $ P.selectList [] []

insertUser :: WithDatabase env m => User -> m (P.Key User)
insertUser user = runDB $ P.insert user

getUserById :: WithDatabase env m => P.Key User -> m (Maybe (Entity User))
getUserById userId = runDB $ P.selectFirst [UserId P.==. userId] []

getUserByEmail :: WithDatabase env m => Email -> m (Maybe (Entity User))
getUserByEmail email = runDB $ P.selectFirst [UserEmail P.==. email] []

getUserByName :: WithDatabase env m => Name -> m (Maybe (Entity User))
getUserByName name = runDB $ P.selectFirst [UserName P.==. name] []

updateUserActivatedValue :: WithDatabase env m => Email -> Name -> m ()
updateUserActivatedValue email name =
  runDB $
    P.updateWhere
      [ UserEmail P.==. email
      , UserName P.==. name
      , UserActivated P.==. Just False
      ]
      [UserActivated P.=. Just True]
