{-# LANGUAGE OverloadedStrings #-}

module Email where

import RIO
import qualified RIO.Text.Lazy as TL
import qualified RIO.HashMap as HashMap
import Network.Mail.SMTP hiding (htmlPart)
import Network.Mail.Mime (htmlPart, plainPart)
import Text.Ginger
import Text.Ginger.Html (htmlSource)
import Model

sendActivationLink :: User -> IO ()
sendActivationLink user = do
  let mail = simpleMail from to cc bcc subject [body, urlHtml]
  sendMailWithLoginTLS host "ninifaroux@gmail.com" "vqzidaqwusrrvjjm" mail
  where
    host       = "smtp.gmail.com"
    from       = Address Nothing "ninifaroux@gmail.com"
    to         = [Address (Just $ userName user) (userEmail user)]
    cc         = []
    bcc        = []
    subject    = "SomeAPI Account Activation"
    body       = plainPart ""
    urlHtml    = htmlPart $ TL.fromStrict (urlText user)

urlText :: User -> Text
urlText user = renderTemplate template (context user)

renderTemplate :: Template SourcePos -> HashMap VarName Text -> Text
renderTemplate template contextMap =
  let contextLookup = flip scopeLookup contextMap
      context = makeContextHtml contextLookup
  in htmlSource $ runGinger context template

template :: Template SourcePos
template =
  either (error . show) id . runIdentity $ parseGinger nullResolver Nothing form
  where
    form = "<form method=post action=http://localhost:8000/activate>" ++
              "<input type=hidden name=userName value={{ name }}>" ++
              "<input type=hidden name=userEmail value={{ email }}>" ++
              "<button type=submit\">Activate</button>" ++
           "</form>"

context :: User -> HashMap Text Text
context user = HashMap.fromList [("name", userName user), ("email", userEmail user)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context = toGVal $ HashMap.lookup key context

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
