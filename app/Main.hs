{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Servant (serve)
import Api (noteApi)
import App (Config(..), CommandOptions(..), Environment(..))
import Web.Server (hoistAppServer)
import Web.Model (runMigrations, initialConfig)
import Docs.Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, help, helper, header, info, long, short, switch, fullDesc, progDesc)

main :: IO ()
main = execParser options >>= runApp
  where
    parser =
      Options
      <$> switch (short 'l' <> long "local" <> help "Run local docker setup instead of production setup")
      <*> switch (short 'd' <> long "docs" <> help "Write the swagger docs for current API")
    options = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Some JSON API"
     <> header "SomeAPI" )

runApp :: CommandOptions -> IO ()
runApp options
  | writeDocs' = writeSwaggerJSON
  | localRun' = run' Local
  | otherwise = run' FlyProduction
  where
    writeDocs' = writeDocs options
    localRun' = localRun options

run' :: Environment -> IO ()
run' environment = do
  _ <- runMigrations
  Config {..} <- initialConfig environment
  run port $ serve noteApi $ hoistAppServer $ Config connectionPool port hostName logFunc
