module Main (main) where

import Servant (serve)
import Api (noteApi)
import Server (hoistAppServer)
import App (Config(..), Environment(..), CommandOptions(..))
import Model (runMigrations, initialConfig)
import Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, help, info, long, short, switch)

main :: IO ()
main = execParser options >>= runApp
  where
    parser = Options <$> switch (short 'l' <> long "local" <> help "run app with local setup instead of docker")
    options = info parser mempty

runApp :: CommandOptions -> IO ()
runApp options
  | devType' = writeSwaggerJSON >> run' Local
  | otherwise = run' Docker 
  where devType' = devType options

run' :: Environment -> IO ()
run' devType = do
  _ <- runMigrations devType
  Config {..} <- initialConfig devType
  run port $ serve noteApi $ hoistAppServer $ Config connectionPool port
