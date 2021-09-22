{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model where

import RIO
import qualified Database.Persist.TH as PTH
import Database.Persist (Entity(..))
import Database.Persist.Sql (Key(..), toSqlKey)
import Database.Persist.Postgresql 
  (ConnectionPool, ConnectionString, SqlPersistT, withPostgresqlConn, runSqlPool, runMigration, createPostgresqlPool)
import Control.Monad.Logger (LoggingT(..), runStdoutLoggingT)
import Data.Aeson
import Data.Password.Bcrypt
import Data.Password.Instances
import App (App, Env(..))
import Say
import Libjwt.Classes
import Web.HttpApiData

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json
    name Text
    age Int Maybe
    email Text
    activated Bool Maybe
    UniqueEmail email
    deriving Show Read Generic

  Auth
    userId (Key User)
    password (PasswordHash Bcrypt)
    UniqueUserId userId
    deriving Eq
|]

data UserWithPassword =
  UserWithPassword {
    name :: !Text
  , age :: !(Maybe Int)
  , email :: !Text
  , password :: !Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserWithPassword
instance ToJSON UserWithPassword

instance Z.HasField "userName" User Text where
  hasField r = (\x -> r{userName=x}, userName r)
instance Z.HasField "userEmail" User Text where
  hasField r = (\x -> r{userEmail=x}, userEmail r)

data Scope = Scope { protectedAccess :: Bool, privateAccess :: Bool }
  deriving stock (Show, Eq, Generic)
data ScopeField = Protected | Private deriving Eq

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

migrateDB :: IO ()
migrateDB = runAction connectionString (runMigration migrateAll)

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a ->  IO a
runAction connectionString action = runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
  runReaderT action backend

initialEnv :: IO Env
initialEnv = do
  pool <- runStdoutLoggingT $ createPostgresqlPool connectionString 2
  return $ Env { connectionPool = pool, port = 8000 }

connectionString :: ConnectionString
connectionString = "host=127.0.0.1 port=5432 user=ninifaroux dbname=someapi password=password"
