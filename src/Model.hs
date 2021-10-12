{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model 
  ( User(..)
  , UserWithPassword(..)
  , UserLogin(..)
  , Note(..)
  , NoteInput(..)
  , Auth(..)
  , EntityField(..)
  , Key(..)
  , runDB
  , makePassword
  , initialEnv
  , runMigrations
  ) 
  where

import RIO (Text, Generic, runReaderT, asks, liftIO)
import RIO.Time (UTCTime)
import qualified Database.Persist.TH as PTH
import Database.Persist.Sql (Key, EntityField)
import Database.Persist.Postgresql
  (ConnectionString, SqlPersistT, withPostgresqlConn, runSqlPool, runMigration, createPostgresqlPool)
import Control.Monad.Logger (LoggingT(..), runStdoutLoggingT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Password.Bcrypt (PasswordHash(..), Bcrypt, hashPassword, mkPassword)
import Data.Password.Instances()
import App (App, Env(..))
import Say (say)
import UserTypes (Name, Email, Age)
import NoteTypes (NoteTitle, NoteBody)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json
    name Name
    age Age
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
    dayCreated Text
    deriving Eq Show Generic
|]

-- | Input type for creating user
data UserWithPassword =
  UserWithPassword {
    name :: !Text
  , age :: !Int
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

makePassword :: Text -> IO (PasswordHash Bcrypt)
makePassword = hashPassword . mkPassword

runDB :: SqlPersistT IO a -> App a
runDB query = do
  connPool <- asks connectionPool
  liftIO $ runSqlPool query connPool

runMigrations :: IO ()
runMigrations = do
  say "Running migrations"
  runAction connectionString $ runMigration migrateAll
  say "Migrations Finished"

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connString action = runStdoutLoggingT $ withPostgresqlConn connString $ \backend ->
  runReaderT action backend

initialEnv :: IO Env
initialEnv = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connectionString 2
  return $ Env { connectionPool = pool, port = 8000 }

connectionString :: ConnectionString
connectionString = "host=postgres-server port=5432 user=postgres dbname=someapi password=password" 

-- connectionString :: ConnectionString
-- connectionString = "host=127.0.0.1 port=5432 user=ninifaroux dbname=someapi password=password"
