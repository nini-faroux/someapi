{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module App 
  ( App
  , Config(..)
  , Environment(..)
  , CommandOptions(..)
  , makeConfig
  ) where

import RIO
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai.Handler.Warp (Port)
import Control.Monad.Logger (runStdoutLoggingT)
import System.Environment (getEnv)
import qualified Data.ByteString.Char8 as LC

type App = RIO Config

data Config = Config
  { connectionPool :: !ConnectionPool
  , connectionString :: !ConnectionString
  , appPort :: !Port
  , appHostName :: !Text
  , dbPort :: !String
  , dbUser :: !String
  , dbName :: !String
  , dbHostName :: !String
  , deployHostName :: !Text
  , logFunc :: !LogFunc
  }

instance HasLogFunc Config where
  logFuncL = lens logFunc (\c f -> c { logFunc = f })

makeConfig :: Environment -> IO Config 
makeConfig environment = do
  postgresPass <- getEnv "POSTGRES_PASSWORD"
  pool' <- makePool postgresPass
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \logFunc' ->
    return Config { 
        connectionPool = pool'
      , connectionString = connectionString' postgresPass
      , appPort = 8080
      , appHostName = hostName'
      , dbPort = dbPort'
      , dbUser = dbUser'
      , dbName = dbName'
      , dbHostName = dbHostName'
      , deployHostName = hostName'
      , logFunc = logFunc' 
      }
  where
    hostName'
      | environment == Local = "http://localhost:8080/"
      | otherwise = "https://some-api.fly.dev/"
    dbPort' = "5432"
    dbUser' = "postgres"
    dbName' = "someapi"
    dbHostName' = "postgres-server.internal"
    makePool :: String -> IO ConnectionPool
    makePool pass = 
      runStdoutLoggingT $ createPostgresqlPool (connectionString' pass) 2
    connectionString' :: String -> ConnectionString
    connectionString' pass = 
       "host=" <> LC.pack dbHostName'
       <> " port=" <> LC.pack dbPort'
       <> " user=" <> LC.pack dbUser'
       <> " dbname=" <> LC.pack dbName'
       <> " password=" <> LC.pack pass

data Environment =
    Local
  | FlyProduction
  deriving (Eq, Show)

data CommandOptions =
  Options {
    localRun :: Bool
  , writeDocs :: Bool
  }
