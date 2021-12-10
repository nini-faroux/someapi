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
  getUserId,
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
  Value,
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
import Parse.NoteTypes (
  NoteBody,
  NoteTitle,
 )
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
  NoteResponse (..),
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

getNotesByName :: WithDatabase env m => Name -> m [NoteResponse]
getNotesByName name = runDB $ do
  notes <- select $ do
    (note :& user) <-
      from $
        table @Note `InnerJoin` table @User
          `on` ( \(note' :& user') ->
                  user' ^. UserId ==. note' ^. NoteUserId
               )
    where_ (user ^. UserName ==. val name)
    pure
      ( note ^. NoteNoteTitle
      , note ^. NoteNoteBody
      , note ^. NoteDateCreated
      , user ^. UserName
      )
  pure $ makeNoteResponses notes

getNotesBetweenDatesWithName ::
  WithDatabase env m =>
  Name ->
  Text ->
  Text ->
  m [NoteResponse]
getNotesBetweenDatesWithName name start end = runDB $ do
  notes <- select $ do
    (note :& user) <-
      from $
        table @Note `InnerJoin` table @User
          `on` ( \(note' :& user') ->
                  user' ^. UserId ==. note' ^. NoteUserId
               )
    where_ (user ^. UserName ==. val name)
    where_ (note ^. NoteDateCreated >=. val start)
    where_ (note ^. NoteDateCreated <=. val end)
    pure
      ( note ^. NoteNoteTitle
      , note ^. NoteNoteBody
      , note ^. NoteDateCreated
      , user ^. UserName
      )
  pure $ makeNoteResponses notes

getNotesBetweenDates ::
  WithDatabase env m =>
  Text ->
  Text ->
  m [NoteResponse]
getNotesBetweenDates start end = runDB $ do
  notes <- select $ do
    (note :& user) <-
      from $
        table @Note `InnerJoin` table @User
          `on` ( \(note' :& user') ->
                  user' ^. UserId ==. note' ^. NoteUserId
               )
    where_ (note ^. NoteDateCreated >=. val start)
    where_ (note ^. NoteDateCreated <=. val end)
    pure
      ( note ^. NoteNoteTitle
      , note ^. NoteNoteBody
      , note ^. NoteDateCreated
      , user ^. UserName
      )
  pure $ makeNoteResponses notes

getNotes ::
  WithDatabase env m =>
  m [NoteResponse]
getNotes = runDB $ do
  notes <- select $ do
    (note :& user) <-
      from $
        table @Note `InnerJoin` table @User
          `on` ( \(note' :& user') ->
                  user' ^. UserId ==. note' ^. NoteUserId
               )
    pure
      ( note ^. NoteNoteTitle
      , note ^. NoteNoteBody
      , note ^. NoteDateCreated
      , user ^. UserName
      )
  pure $ makeNoteResponses notes

makeNoteResponses ::
  [(Value NoteTitle, Value NoteBody, Value Text, Value Name)] ->
  [NoteResponse]
makeNoteResponses = foldr ((:) . noteResponse) []
  where
    noteResponse (noteTitle, noteBody, dateCreated, userName) =
      NoteResponse
        (unValue noteTitle)
        (unValue noteBody)
        (unValue dateCreated)
        (unValue userName)

getUserId :: WithDatabase env m => Name -> m (Maybe (P.Key User))
getUserId name = runDB $ do
  userIds <- select $ do
    user <- from $ table @User
    where_ (user ^. UserName ==. val name)
    pure $ user ^. UserId
  case userIds of
    [] -> pure Nothing
    (userId : _) -> pure $ Just $ unValue userId

getFirstDate :: WithDatabase env m => m (Maybe Text)
getFirstDate = runDB $ do
  days <- select $ do
    note <- from $ table @Note
    pure $ min_ (note ^. NoteDateCreated)
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
