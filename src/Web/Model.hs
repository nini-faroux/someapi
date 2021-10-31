{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ConstraintKinds            #-}

module Web.Model 
  ( User(..)
  , UserWithPassword(..)
  , UserLogin(..)
  , Note(..)
  , NoteInput(..)
  , Auth(..)
  , EntityField(..)
  , Key(..)
  , RunPool(..)
  , Database
  , runDB
  , runMigrations
  ) 
  where

import RIO
import RIO.Time (UTCTime)
import qualified Database.Persist.TH as PTH
import Database.Persist.Sql (Key, EntityField)
import Database.Persist.Postgresql
  (ConnectionString, SqlPersistT, SqlBackend, withPostgresqlConn, runSqlPool, runMigration)
import Data.Pool (Pool)
import Control.Monad.Logger (LoggingT(..), runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Bcrypt (PasswordHash(..), Bcrypt)
import Data.Password.Instances()
import App (App, HasConnectionPool(..))
import Say (say)
import Parse.UserTypes (Name, Email)
import Parse.NoteTypes (NoteTitle, NoteBody)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json
    name Name
    email Email
    activated Bool Maybe
    UniqueEmail email
    UniqueName name
    deriving Eq Show Read Generic

  Auth
    userId (Key User)
    password (PasswordHash Bcrypt)
    UniqueUserId userId
    deriving Eq

  Note json
    userName Name
    noteTitle NoteTitle
    noteBody NoteBody
    timeCreated UTCTime
    dateCreated Text
    deriving Eq Show Generic
|]

-- | Input type for creating user
data UserWithPassword =
  UserWithPassword {
    name :: !Text
  , email :: !Text
  , password :: !Text
  } deriving (Eq, Show, Generic)
instance FromJSON UserWithPassword
instance ToJSON UserWithPassword

-- | Input type for user login
data UserLogin =
  UserLogin {
    loginName :: !Text
  , loginPassword :: !Text
  } deriving (Eq, Show, Generic)
instance FromJSON UserLogin
instance ToJSON UserLogin

-- | Input type for the body of
-- a new user note
data NoteInput =
  NoteInput {
    noteAuthor :: !Text
  , noteTitle :: !Text
  , noteBody :: !Text
  } deriving (Eq, Show, Generic)
instance FromJSON NoteInput
instance ToJSON NoteInput

instance Z.HasField "userName" User Name where
  hasField r = (\x -> r{userName=x}, userName r)
instance Z.HasField "userEmail" User Email where
  hasField r = (\x -> r{userEmail=x}, userEmail r)

class Monad m => RunPool m where
  runPool :: SqlPersistT IO a -> Pool SqlBackend -> m a
instance RunPool App where
  runPool query env = liftIO $ runSqlPool query env

type Database env m = (HasConnectionPool env, MonadReader env m, RunPool m)

runDB :: Database env m => SqlPersistT IO a -> m a
runDB query = do
  config <- ask
  runPool query $ getConnectionPool config

runMigrations :: ConnectionString -> IO ()
runMigrations connectionString = do
  say "Running migrations"
  runAction connectionString $ runMigration migrateAll
  say "Migrations Finished"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend
