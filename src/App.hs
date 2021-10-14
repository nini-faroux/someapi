module App 
  ( App
  , Config(..)
  , Environment(..)
  , CommandOptions(..)
  ) where

import RIO
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)

type App = RIO Config

data Config = Config
  { connectionPool :: !ConnectionPool
  , port :: !Port
  }

data Environment =
    Local
  | Docker
  deriving (Eq, Show)

newtype CommandOptions =
  Options {
    envType :: Bool
  }
