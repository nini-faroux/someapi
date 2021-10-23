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

sendActivationLink :: User -> IO ()
sendActivationLink user = do
  now <- getCurrentTime
  token <- makeUserToken user now
  (googleMail, googleMail', googlePass) <- getEnvVars
  let urlHtml = htmlPart $ TL.fromStrict $ urlText token
      mail = simpleMail (from googleMail') to cc bcc subject [body, urlHtml]
  sendMailWithLoginTLS host googleMail googlePass mail
  where
    host       = "smtp.gmail.com"
    from       = Address Nothing
    to         = [Address (Just (renderName user.userName)) (renderEmail user.userEmail)]
    cc         = []
    bcc        = []
    subject    = "SomeAPI Account Activation"
    body       = plainPart ""
    urlText token =
      case decodeUtf8' token of
        Left _err -> error "Utf8 decoding error"
        Right token' -> renderTokenTemplate tokenTemplate $ context token'

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
      form = "<form method=post action=https://some-api.fly.dev/activate>" ++
                "<input type=hidden name=token value={{ token }}>" ++
                "<button type=submit\">Activate</button>" ++
              "</form>"

context :: Text -> HashMap Text Text
context token = HashMap.fromList [("token", token)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context' = toGVal $ HashMap.lookup key context'

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
