{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module App (
  App (..),
  AppConfig (..),
  Config (..),
  CommandOptions (..),
  DBConfig (..),
  EmailConfig (..),
  HasAppHostName (..),
  HasConnectionPool (..),
  HasGoogleMail (..),
  HasSecret (..),
  WithTime (..),
  makeConfig,
) where

import Colog (
  HasLog (..),
  LogAction,
  Message,
  richMessageAction,
 )
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8 as LC
import Data.Generics.Product (field)
import Database.Persist.Postgresql (
  ConnectionPool,
  ConnectionString,
  createPostgresqlPool,
 )
import Environment (
  EnvVars,
  getEnvVars,
 )
import qualified Environment as E
import Network.Wai.Handler.Warp (Port)
import RIO
import RIO.Time (
  UTCTime,
  getCurrentTime,
 )

newtype App a = App
  {unApp :: ReaderT (Config App) IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (Config App)
    )

data Config m = Config
  { appConfig :: !(AppConfig m)
  , dbConfig :: !DBConfig
  , emailConfig :: !EmailConfig
  }
  deriving (Generic)

data AppConfig m = AppConfig
  { appPort :: !Port
  , appHostName :: !Text
  , configLogAction :: !(LogAction m Message)
  }
  deriving (Generic)

data DBConfig = DBConfig
  { connectionPool :: !ConnectionPool
  , connectionString :: !ConnectionString
  , postgresDb :: !String
  , dbPort :: !String
  , postgresPass :: !String
  , postgresUser :: !String
  }
  deriving (Generic)

data EmailConfig = EmailConfig
  { googleMail :: !String
  , googlePass :: !String
  , hmacSecret :: !String
  }
  deriving (Generic)

makeConfig :: MonadIO m => IO (Config m)
makeConfig = do
  envVars <- getEnvVars
  pool' <- makePool envVars
  return
    Config
      { appConfig =
          AppConfig
            { appPort = 8080
            , appHostName = E.appHostName envVars
            , configLogAction = richMessageAction
            }
      , dbConfig =
          DBConfig
            { connectionPool = pool'
            , connectionString = connectionString' envVars
            , postgresDb = postgresDb' envVars
            , dbPort = dbPort'
            , postgresUser = postgresUser' envVars
            , postgresPass = postgresPass' envVars
            }
      , emailConfig =
          EmailConfig
            { googleMail = E.googleMail envVars
            , googlePass = E.googlePass envVars
            , hmacSecret = E.hmacSecret envVars
            }
      }
  where
    makePool :: EnvVars -> IO ConnectionPool
    makePool evs =
      runStdoutLoggingT $ createPostgresqlPool (connectionString' evs) 2
    connectionString' :: EnvVars -> ConnectionString
    connectionString' evs =
      "host=" <> LC.pack "postgres-server.internal"
        <> " port="
        <> LC.pack dbPort'
        <> " user="
        <> LC.pack (postgresUser' evs)
        <> " dbname="
        <> LC.pack (postgresDb' evs)
        <> " password="
        <> LC.pack (postgresPass' evs)
    dbPort' = "5432"
    postgresUser' = E.postgresUser
    postgresDb' = E.postgresDb
    postgresPass' = E.postgresPass

instance HasLog (Config m) Message m where
  getLogAction :: Config m -> LogAction m Message
  getLogAction = configLogAction . appConfig

  setLogAction :: LogAction m Message -> Config m -> Config m
  setLogAction newLogAction config =
    config & field @"appConfig" . field @"configLogAction" .~ newLogAction

class HasConnectionPool env where
  getConnectionPool :: env -> ConnectionPool

instance HasConnectionPool (Config m) where
  getConnectionPool = connectionPool . dbConfig

class HasAppHostName env where
  getAppHostName :: env -> Text

instance HasAppHostName (Config m) where
  getAppHostName = appHostName . appConfig

class HasGoogleMail env where
  getGoogleMail :: env -> String
  getGooglePass :: env -> String

instance HasGoogleMail (Config m) where
  getGoogleMail = googleMail . emailConfig
  getGooglePass = googlePass . emailConfig

class HasSecret env where
  getHmacSecret :: env -> String

instance HasSecret (Config m) where
  getHmacSecret = hmacSecret . emailConfig

class Monad m => WithTime m where
  getTime :: m UTCTime

instance WithTime App where
  getTime = liftIO getCurrentTime

-- | For tests
instance WithTime IO where
  getTime = getCurrentTime

newtype CommandOptions = Options
  { writeDocs :: Bool
  }
