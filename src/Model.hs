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
  , Scope(..)
  , ScopeField(..)
  , Token(..)
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
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import UserTypes (Name, Email, Age)

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json
    name Name
    age Age
    email Email
    activated Bool Maybe
    UniqueEmail email
    deriving Eq Show Read Generic

  Auth
    userId (Key User)
    password (PasswordHash Bcrypt)
    UniqueUserId userId
    deriving Eq

  Note json
    userId (Key User)
    userName Name
    noteBody Text
    timeCreated UTCTime
    deriving Eq Show Generic
|]

data NoteInput =
  NoteInput {
    userId :: !(Key User)
  , noteName :: !Text
  , noteBody :: !Text
  } deriving (Eq, Show, Generic)

data UserWithPassword =
  UserWithPassword {
    name :: !Text
  , age :: !Int
  , email :: !Text
  , password :: !Text
  } deriving (Eq, Show, Generic)

data UserLogin =
  UserLogin {
    loginEmail :: !Text
  , loginPassword :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserWithPassword
instance FromJSON UserLogin
instance FromJSON NoteInput
instance ToJSON UserWithPassword
instance ToJSON UserLogin
instance ToJSON NoteInput

instance Z.HasField "userName" User Name where
  hasField r = (\x -> r{userName=x}, userName r)
instance Z.HasField "userEmail" User Email where
  hasField r = (\x -> r{userEmail=x}, userEmail r)

data Scope = Scope { protectedAccess :: Bool, adminAccess :: Bool }
  deriving stock (Show, Eq, Generic)
data ScopeField = Protected | Admin deriving Eq

newtype Token = Token { token :: Text }
  deriving (Eq, Show, Generic, FromHttpApiData, ToHttpApiData)

instance FromJSON Token
instance ToJSON Token

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
connectionString = "host=127.0.0.1 port=5432 user=ninifaroux dbname=someapi password=password"
