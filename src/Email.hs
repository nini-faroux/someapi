{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Email where

import Servant.Auth.Server
import RIO
import RIO.Time
import qualified RIO.Text.Lazy as TL
import qualified RIO.HashMap as HashMap
import Network.Mail.SMTP hiding (htmlPart)
import Network.Mail.Mime (htmlPart, plainPart)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Model
import Config
import JWT

sendActivationLink :: User -> IO ()
sendActivationLink user = do
  now <- getCurrentTime
  let token = makeToken user now
  let urlHtml = htmlPart $ TL.fromStrict $ urlText user token
      mail = simpleMail from to cc bcc subject [body, urlHtml]
  sendMailWithLoginTLS host googleMail googlePass mail
  where
    host       = "smtp.gmail.com"
    from       = Address Nothing googleMail'
    to         = [Address (Just $ user.userName) (user.userEmail)]
    cc         = []
    bcc        = []
    subject    = "SomeAPI Account Activation"
    body       = plainPart ""
    urlText user token =
      case decodeUtf8' token of
        Left err -> error "Utf8 decoding error"
        Right token' -> renderTokenTemplate (tokenTemplate token') (context token')

renderTokenTemplate :: Template SourcePos -> HashMap Text Text -> Text
renderTokenTemplate template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
   in htmlSource $ runGinger context template

tokenTemplate :: Text -> Template SourcePos
tokenTemplate token =
  either (error . show) id . runIdentity $ parseGinger nullResolver Nothing form
    where
      form = "<form method=post action=http://localhost:8000/activate>" ++
                "<input type=hidden name=token value={{ token }}>" ++
                "<button type=submit\">Activate</button>" ++
              "</form>"

context :: Text -> HashMap Text Text
context token = HashMap.fromList [("token", token)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
