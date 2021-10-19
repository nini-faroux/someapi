{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import RIO
import Servant (serve)
import Api (noteApi)
import App (Config(..), CommandOptions(..))
import Web.Server (hoistAppServer)
import Web.Model (runMigrations, initialConfig)
import Docs.Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, help, helper, header, info, long, short, switch, fullDesc, progDesc)

main :: IO ()
main = execParser options >>= runApp
  where
    parser = Options <$> switch (short 'd' <> long "docs" <> help "Write the swagger docs for current API")
    options = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Some JSON API"
     <> header "SomeAPI" )

runApp :: CommandOptions -> IO ()
runApp options
  | writeDocs' = writeSwaggerJSON
  | otherwise = run'
  where writeDocs' = writeDocs options

run' :: IO ()
run' = do
  _ <- runMigrations
  Config {..} <- initialConfig
  run port $ serve noteApi $ hoistAppServer $ Config connectionPool port logFunc
