{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Email (sendActivationLink) where

import RIO hiding (to)
import qualified RIO.Text.Lazy as TL
import qualified RIO.HashMap as HashMap
import RIO.Time (getCurrentTime)
import RIO.Text (pack)
import Text.Ginger (
  Template, SourcePos, GVal, ToGVal, IncludeResolver, toGVal, makeContextHtml, runGinger, parseGinger)
import Text.Ginger.Html (htmlSource)
import Network.Mail.SMTP hiding (htmlPart)
import Network.Mail.Mime (htmlPart, plainPart)
import System.Environment (getEnv)
import Web.Model (User)
import Web.JWT (makeUserToken)
import Parse.UserTypes (renderEmail, renderName)
import App (App, Config(..))

sendActivationLink :: User -> App ()
sendActivationLink user = do
  Config {..} <- ask
  now <- liftIO getCurrentTime
  token <- liftIO $ makeUserToken user now
  (googleMail, googleMail', googlePass) <- liftIO getEnvVars
  let urlHtml = htmlPart $ TL.fromStrict $ urlText token hostName
      mail = simpleMail (from googleMail') to cc bcc subject [body, urlHtml]
  liftIO $ sendMailWithLoginTLS host googleMail googlePass mail
  where
    host       = "smtp.gmail.com"
    from       = Address Nothing
    to         = [Address (Just (renderName user.userName)) (renderEmail user.userEmail)]
    cc         = []
    bcc        = []
    subject    = "SomeAPI Account Activation"
    body       = plainPart ""
    urlText token hostName' =
      case decodeUtf8' token of
        Left _err -> error "Utf8 decoding error"
        Right token' -> renderTokenTemplate tokenTemplate $ context token' hostName'

getEnvVars :: IO (UserName, Text, Password)
getEnvVars = do
  gmail <- getEnv "GOOGLE_MAIL"
  pass <- getEnv "GOOGLE_PASS"
  return (gmail, pack gmail, pass)

renderTokenTemplate :: Template SourcePos -> HashMap Text Text -> Text
renderTokenTemplate template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context' = makeContextHtml contextLookup
   in htmlSource $ runGinger context' template

tokenTemplate :: Template SourcePos
tokenTemplate =
  either (error . show) id . runIdentity $ parseGinger nullResolver Nothing form
    where
      form = 
        "<form method=post action={{ hostName }}" ++ "activate>" ++
           "<input type=hidden name=token value={{ token }}>" ++
           "<button type=submit\">Activate</button>" ++
        "</form>"

context :: Text -> Text -> HashMap Text Text
context token host = HashMap.fromList [("token", token), ("hostName", host)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context' = toGVal $ HashMap.lookup key context'

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
