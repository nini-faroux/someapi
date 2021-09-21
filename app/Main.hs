{-# LANGUAGE RecordWildCards #-}

module Main where

import RIO
import Servant
import Api
import Server
import App
import Model
import Docs
import Network.Wai.Handler.Warp (run)
import Control.Monad.Logger
import Database.Persist.Postgresql

main :: IO ()
main = do
  writeSwaggerJSON
  Env {..} <- initialEnv
  _ <- runMigrations
  run port $ serve userApi $ hoistAppServer $ Env connectionPool port
