{-# LANGUAGE NoImplicitPrelude #-}

module App (App, Env(..)) where

import RIO (RIO)
import Database.Persist.Postgresql (ConnectionPool)
import Network.Wai.Handler.Warp (Port)

type App = RIO Env

data Env = Env
  { connectionPool :: !ConnectionPool
  , port :: !Port
  }
