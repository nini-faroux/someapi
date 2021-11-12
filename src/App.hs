module App (
  App,
  CommandOptions (..),
  Config (..),
  AppConfig (..),
  DBConfig (..),
  EmailConfig (..),
  HasGoogleMail (..),
  HasSecret (..),
  WithEnv (..),
  WithTime (..),
  HasAppHostName (..),
  HasConnectionPool (..),
  makeConfig,
) where

import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8 as LC
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai.Handler.Warp (Port)
import RIO
import RIO.Time (UTCTime, getCurrentTime)
import System.Environment (getEnv)
import Environment (EnvVars, getEnvVars)
import qualified Environment as E

type App = RIO Config

data Config =
  Config {
    appConfig :: !AppConfig
  , dbConfig :: !DBConfig
  , emailConfig :: !EmailConfig
  }

data AppConfig =
  AppConfig {
    appPort :: !Port
  , appHostName :: !Text
  }

data DBConfig =
  DBConfig {
    connectionPool :: !ConnectionPool
  , connectionString :: !ConnectionString
  , postgresDb :: !String
  , dbPort :: !String
  , postgresPass :: !String
  , postgresUser :: !String
  }

data EmailConfig =
  EmailConfig {
    googleMail :: !String
  , googlePass :: !String
  , hmacSecret :: !String
  }

makeConfig :: IO Config
makeConfig = do
  envVars <- getEnvVars
  pool' <- makePool envVars
  return Config {
     appConfig = 
       AppConfig {
         appPort = 8080
       , appHostName = E.appHostName envVars
       }, 
     dbConfig = 
       DBConfig {
           connectionPool = pool'
         , connectionString = connectionString' envVars
         , postgresDb = postgresDb' envVars
         , dbPort = dbPort'
         , postgresUser = postgresUser' envVars
         , postgresPass = postgresPass' envVars
       },
     emailConfig =
       EmailConfig {
         googleMail = E.googleMail envVars
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

class HasConnectionPool env where
  getConnectionPool :: env -> ConnectionPool

instance HasConnectionPool Config where
  getConnectionPool = connectionPool . dbConfig

class HasAppHostName env where
  getAppHostName :: env -> Text

instance HasAppHostName Config where
  getAppHostName = appHostName . appConfig

class HasGoogleMail env where
  getGoogleMail :: env -> String
  getGooglePass :: env -> String

instance HasGoogleMail Config where
  getGoogleMail = googleMail . emailConfig
  getGooglePass = googlePass . emailConfig

class HasSecret env where
  getHmacSecret :: env -> String

instance HasSecret Config where
  getHmacSecret = hmacSecret . emailConfig

class Monad m => WithTime m where
  getTime :: m UTCTime

instance WithTime App where
  getTime = liftIO getCurrentTime

-- | For tests
instance WithTime IO where
  getTime = getCurrentTime

class Monad m => WithEnv m where
  getEnv' :: String -> m String

instance WithEnv App where
  getEnv' var = liftIO $ getEnv var

newtype CommandOptions = Options
  { writeDocs :: Bool
  }
