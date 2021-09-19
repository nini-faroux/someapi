{-# LANGUAGE RecordWildCards #-}

module Main where

import RIO
import Servant
import Api
import App
import Model
import Docs
import Network.Wai.Handler.Warp (run)
import Control.Monad.Logger
import Database.Persist.Postgresql
import Servant.Docs

main :: IO ()
main = do
  putStrLn $ markdown apiDocs
  Env {..} <- initialEnv
  _ <- runMigrations
  run port $ serve userApi $ hoistAppServer $ Env connectionPool port
