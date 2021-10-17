module Main (main) where

import Servant (serve)
import Api (noteApi)
import App (Config(..), Environment(..), CommandOptions(..))
import Web.Server (hoistAppServer)
import Web.Model (runMigrations, initialConfig)
import Docs.Docs (writeSwaggerJSON)
import Network.Wai.Handler.Warp (run)
import Options.Applicative (execParser, help, info, long, short, switch)

main :: IO ()
main = execParser options >>= runApp
  where
    parser = Options <$> switch (short 'l' <> long "local" <> help "run app with local setup instead of docker")
    options = info parser mempty

runApp :: CommandOptions -> IO ()
runApp options
  | isLocalEnv = writeSwaggerJSON >> run' Local
  | otherwise = run' Docker 
  where isLocalEnv = envType options

run' :: Environment -> IO ()
run' envType = do
  _ <- runMigrations envType
  Config {..} <- initialConfig envType
  run port $ serve noteApi $ hoistAppServer $ Config connectionPool port logFunc
