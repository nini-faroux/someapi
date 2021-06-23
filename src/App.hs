{-# LANGUAGE NoImplicitPrelude #-}

module App where

import RIO
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool)
import Network.Wai.Handler.Warp

type App = RIO Env

data Env = Env
  { connectionPool :: !ConnectionPool
  , port :: !Port
  }
