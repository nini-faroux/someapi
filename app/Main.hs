{-# LANGUAGE RecordWildCards #-}

module Main where

import RIO
import Servant
import Api
import App
import Model
import Network.Wai.Handler.Warp (run)
import Control.Monad.Logger
import Database.Persist.Postgresql

main :: IO ()
main = do
  Env {..} <- initialEnv
  _ <- runMigrations
  run port $ serve proxyAPI $ hoistAppServer $ Env connectionPool port
