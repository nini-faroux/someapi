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
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)

type App = RIO Config

data Config = Config
  { connectionPool :: !ConnectionPool
  , port :: !Port
  , hostName :: !Text
  , logFunc :: !LogFunc
  }

instance HasLogFunc Config where
  logFuncL = lens logFunc (\c f -> c { logFunc = f })

makeConfig :: Environment -> ConnectionPool -> IO Config 
makeConfig environment pool = do
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \logFunc' ->
    return Config { connectionPool = pool, port = 8080, hostName = hostName', logFunc = logFunc' }
  where
    hostName'
      | environment == Local = "http://localhost:8080/"
      | otherwise = "https://some-api.fly.dev/"

data Environment =
    Local
  | FlyProduction
  deriving (Eq, Show)

data CommandOptions =
  Options {
    localRun :: Bool
  , writeDocs :: Bool
  }
