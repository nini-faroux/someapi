{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Email (sendActivationLink) where

import Text.Ginger (
  Template, SourcePos, GVal, ToGVal, IncludeResolver, toGVal, makeContextHtml, runGinger, parseGinger)
import Text.Ginger.Html (htmlSource)
import RIO (Text, HashMap, Identity, Hashable, decodeUtf8', runIdentity)
import RIO.Time (getCurrentTime)
import qualified RIO.Text.Lazy as TL
import qualified RIO.HashMap as HashMap
import Network.Mail.SMTP hiding (htmlPart)
import Network.Mail.Mime (htmlPart, plainPart)
import Model (User)
import Config (googleMail, googleMail', googlePass)
import JWT (makeUserToken)
import UserTypes (renderEmail, renderName)

sendActivationLink :: User -> IO ()
sendActivationLink user = do
  now <- getCurrentTime
  let token = makeUserToken user now
  let urlHtml = htmlPart $ TL.fromStrict $ urlText token
      mail = simpleMail from to cc bcc subject [body, urlHtml]
  sendMailWithLoginTLS host googleMail googlePass mail
  where
    host       = "smtp.gmail.com"
    from       = Address Nothing googleMail'
    to         = [Address (Just (renderName user.userName)) (renderEmail user.userEmail)]
    cc         = []
    bcc        = []
    subject    = "SomeAPI Account Activation"
    body       = plainPart ""
    urlText token =
      case decodeUtf8' token of
        Left _err -> error "Utf8 decoding error"
        Right token' -> renderTokenTemplate tokenTemplate $ context token'

renderTokenTemplate :: Template SourcePos -> HashMap Text Text -> Text
renderTokenTemplate template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context' = makeContextHtml contextLookup
   in htmlSource $ runGinger context' template

tokenTemplate :: Template SourcePos
tokenTemplate =
  either (error . show) id . runIdentity $ parseGinger nullResolver Nothing form
    where
      form = "<form method=post action=http://localhost:8000/activate>" ++
                "<input type=hidden name=token value={{ token }}>" ++
                "<button type=submit\">Activate</button>" ++
              "</form>"

context :: Text -> HashMap Text Text
context token = HashMap.fromList [("token", token)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context' = toGVal $ HashMap.lookup key context'

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
