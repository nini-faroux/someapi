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
  , logFunc :: !LogFunc
  }

instance HasLogFunc Config where
  logFuncL = lens logFunc (\c f -> c { logFunc = f })

makeConfig :: ConnectionPool -> IO Config 
makeConfig pool = do
  logOptions' <- logOptionsHandle stdout False
  let logOptions = setLogUseTime True logOptions'
  withLogFunc logOptions $ \logFunc' ->
    return Config { connectionPool = pool, port = 8080, logFunc = logFunc' }

data Environment =
    Local
  | Docker
  deriving (Eq, Show)

newtype CommandOptions =
  Options {
    writeDocs :: Bool
  }
