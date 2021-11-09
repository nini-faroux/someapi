module Main (main) where

import Api (noteApi)
import App (
  CommandOptions (..),
  Config (..),
  makeConfig,
 )
import Docs.Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  progDesc,
  short,
  switch,
 )
import RIO
import Servant (serve)
import Web.Model (runMigrations)
import Web.Server (hoistAppServer)

main :: IO ()
main = execParser options >>= runApp
  where
    parser =
      Options
        <$> switch (short 'd' <> long "docs" <> help "Write the swagger docs for current API")
    options =
      info
        (helper <*> parser)
        ( fullDesc
            <> progDesc "Some JSON API"
            <> header "SomeAPI"
        )

runApp :: CommandOptions -> IO ()
runApp options
  | writeDocs' = writeSwaggerJSON
  | otherwise = run'
  where
    writeDocs' = writeDocs options
    run' :: IO ()
    run' = do
      cfg@Config {..} <- makeConfig
      _ <- runMigrations connectionString
      run appPort $ serve noteApi $ hoistAppServer cfg
