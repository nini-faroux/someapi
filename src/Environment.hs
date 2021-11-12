module Environment
  ( EnvVars(..)
  , getEnvVars
  )
  where

import RIO
import qualified Data.Text as T
import System.Environment (getEnv)
import GHC.Generics ()

data EnvVars =
  EnvVars {
    postgresUser :: !String
  , postgresPass :: !String
  , postgresDb :: !String
  , appHostName :: !Text
  , googleMail :: !String
  , googlePass :: !String
  , hmacSecret :: !String
  }

getEnvVars :: IO EnvVars
getEnvVars = do
  pgUser <- getEnv "POSTGRES_USER"
  pgPass <- getEnv "POSTGRES_PASSWORD"
  pgDb <- getEnv "POSTGRES_DB"
  host <- getEnv "HOST_NAME"
  gmail <- getEnv "GOOGLE_MAIL"
  pass <- getEnv "GOOGLE_PASS"
  secret <- getEnv "HMAC_SECRET"
  return $
    EnvVars pgUser pgPass pgDb (T.pack host) gmail pass secret
