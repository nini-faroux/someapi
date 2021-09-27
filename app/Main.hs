{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Servant (serve)
import Api (userApi)
import Server (hoistAppServer)
import App (Env(..))
import Model (runMigrations, initialEnv)
import Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  writeSwaggerJSON
  Env {..} <- initialEnv
  _ <- runMigrations
  run port $ serve userApi $ hoistAppServer $ Env connectionPool port
