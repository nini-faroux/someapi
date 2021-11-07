module App (
  App,
  CommandOptions (..),
  Config (..),
  Environment (..),
  GetEnv (..),
  WithTime (..),
  HasAppHostName (..),
  HasConnectionPool (..),
  makeConfig,
) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8 as LC
import Database.Persist.Postgresql (ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai.Handler.Warp (Port)
import RIO
import RIO.Time (UTCTime, getCurrentTime)
import System.Environment (getEnv)

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

class HasConnectionPool env where
  getConnectionPool :: env -> ConnectionPool

instance HasConnectionPool Config where
  getConnectionPool = connectionPool

class HasAppHostName env where
  getAppHostName :: env -> Text

instance HasAppHostName Config where
  getAppHostName = appHostName

instance HasLogFunc Config where
  logFuncL = lens logFunc (\c f -> c {logFunc = f})

makeConfig :: Environment -> IO Config
makeConfig environment = do
  void $ loadFile defaultConfig
  postgresPass <- getEnv "POSTGRES_PASSWORD"
  pool' <- makePool postgresPass
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \logFunc' ->
    return
      Config
        { connectionPool = pool'
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
      | environment == Test = "localhost"
      | otherwise = "https://some-api.fly.dev/"
    dbHostName' = "postgres-server.internal"
    dbPort' = "5432"
    dbUser' = "postgres"
    dbName' = "someapi"
    makePool :: String -> IO ConnectionPool
    makePool pass =
      runStdoutLoggingT $ createPostgresqlPool (connectionString' pass) 2
    connectionString' :: String -> ConnectionString
    connectionString' pass =
      "host=" <> LC.pack dbHostName'
        <> " port="
        <> LC.pack dbPort'
        <> " user="
        <> LC.pack dbUser'
        <> " dbname="
        <> LC.pack dbName'
        <> " password="
        <> LC.pack pass

class Monad m => WithTime m where
  getTime :: m UTCTime

instance WithTime App where
  getTime = liftIO getCurrentTime

-- | For tests
instance WithTime IO where
  getTime = getCurrentTime

class Monad m => GetEnv m where
  getEnv' :: String -> m String

instance GetEnv App where
  getEnv' var = liftIO $ getEnv var

data Environment
  = Local
  | FlyProduction
  | Test
  deriving (Eq, Show)

data CommandOptions = Options
  { localRun :: Bool
  , writeDocs :: Bool
  }
