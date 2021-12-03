{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Model (
  Auth (..),
  WithDatabase,
  EntityField (..),
  Key (..),
  Note (..),
  NoteInput (..),
  RunPool (..),
  User (..),
  UserLogin (..),
  UserWithPassword (..),
  runDB,
  runMigrations,
) where

import App (
  App,
  HasConnectionPool (..),
 )
import Control.Monad.Logger (
  LoggingT (..),
  runStdoutLoggingT,
 )
import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import Data.Password.Bcrypt (
  Bcrypt,
  PasswordHash (..),
 )
import Data.Password.Instances ()
import Data.Pool (Pool)
import Database.Persist.Postgresql (
  ConnectionString,
  SqlBackend,
  SqlPersistT,
  runMigration,
  runSqlPool,
  withPostgresqlConn,
 )
import Database.Persist.Sql (
  EntityField,
  Key,
 )
import qualified Database.Persist.TH as PTH
import Parse.NoteTypes (
  NoteBody,
  NoteTitle,
 )
import Parse.UserTypes (
  Email,
  Name,
 )
import RIO
import RIO.Time (UTCTime)
import Say (say)

PTH.share
  [PTH.mkPersist PTH.sqlSettings,
   PTH.mkMigrate "migrateAll"
  ]
  [PTH.persistLowerCase|
  User json
    name Name
    email Email
    activated Bool Maybe
    UniqueEmail email
    UniqueName name
    deriving Eq Show Read Generic

  Auth
    userId (Key User) OnDeleteCascade
    password (PasswordHash Bcrypt)
    deriving Eq

  Note json
    userName Name
    noteTitle NoteTitle
    noteBody NoteBody
    timeCreated UTCTime
    dateCreated Text

    Foreign User OnDeleteCascade user_name_fk userName References name
    deriving Eq Show Generic
|]

-- | Input type for creating user
data UserWithPassword = UserWithPassword
  { name :: !Text
  , email :: !Text
  , password :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserWithPassword

instance ToJSON UserWithPassword

-- | Input type for user login
data UserLogin = UserLogin
  { loginName :: !Text
  , loginPassword :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserLogin

instance ToJSON UserLogin

{- | Input type for the body of
 a new user note
-}
data NoteInput = NoteInput
  { noteAuthor :: !Text
  , noteTitle :: !Text
  , noteBody :: !Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON NoteInput

instance ToJSON NoteInput

instance Z.HasField "userName" User Name where
  hasField r = (\x -> r{userName = x}, userName r)

instance Z.HasField "userEmail" User Email where
  hasField r = (\x -> r{userEmail = x}, userEmail r)

class Monad m => RunPool m where
  runPool :: SqlPersistT IO a -> Pool SqlBackend -> m a

instance RunPool App where
  runPool query env = liftIO $ runSqlPool query env

type WithDatabase env m = (HasConnectionPool env, MonadReader env m, RunPool m)

runDB :: WithDatabase env m => SqlPersistT IO a -> m a
runDB query = do
  connPool <- asks getConnectionPool
  runPool query connPool

runMigrations :: ConnectionString -> IO ()
runMigrations connectionString = do
  say "Running migrations"
  runAction connectionString $ runMigration migrateAll
  say "Migrations Finished"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = runStdoutLoggingT $
  withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend
