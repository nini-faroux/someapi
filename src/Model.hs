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
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Postgresql 
  (ConnectionPool, ConnectionString, SqlPersistT, withPostgresqlConn, runSqlPool, runMigration, createPostgresqlPool)
import Control.Monad.Logger (LoggingT(..), runStdoutLoggingT)
import App (App, Env(..))
import Say

PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json
    name Text
    age Int
    email Text
    activated Bool Maybe
    UniqueEmail email
    deriving Show Read Generic
|]

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
