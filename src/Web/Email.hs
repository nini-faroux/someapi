{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Web.Email (
  WithMail,
  sendActivationLink,
) where

import App (
  App,
  HasGoogleMail (..),
  HasAppHostName (..),
  WithEnv (..),
  WithTime (..),
 )
import Network.Mail.Mime (
  Mail,
  htmlPart,
  plainPart,
 )
import Network.Mail.SMTP hiding (
  htmlPart,
  sendMail,
 )
import Parse.UserTypes (
  renderEmail,
  renderName,
 )
import RIO hiding (to)
import qualified RIO.HashMap as HashMap
import RIO.Text (pack)
import qualified RIO.Text.Lazy as TL
import Text.Ginger (
  GVal,
  IncludeResolver,
  SourcePos,
  Template,
  ToGVal,
  makeContextHtml,
  parseGinger,
  runGinger,
  toGVal,
 )
import Text.Ginger.Html (htmlSource)
import Web.JWT (UserToken (..))
import Web.Model (User)

class Monad m => SendMail m where
  sendMail :: String -> UserName -> Password -> Mail -> m ()

instance SendMail App where
  sendMail hostName userName pass mail = liftIO $ sendMailWithLoginTLS hostName userName pass mail

type WithMail env m =
  ( HasAppHostName env
  , HasGoogleMail env
  , MonadReader env m
  , SendMail m
  , UserToken m
  , WithEnv m
  , WithTime m
  )

sendActivationLink :: (WithMail env m) => User -> m ()
sendActivationLink user = do
  config <- ask
  now <- getTime
  token <- makeUserToken user now
  (googleMail, googleMail', googlePass) <- getEmailDetails config
  let urlHtml = htmlPart $ TL.fromStrict $ urlText token $ getAppHostName config
      mail = simpleMail (from googleMail') to cc bcc subject [body, urlHtml]
  sendMail host googleMail googlePass mail
  where
    host = "smtp.gmail.com"
    from = Address Nothing
    to = [Address (Just (renderName user.userName)) (renderEmail user.userEmail)]
    cc = []
    bcc = []
    subject = "SomeAPI Account Activation"
    body = plainPart ""
    urlText token appHostName' =
      case decodeUtf8' token of
        Left _err -> error "Utf8 decoding error"
        Right token' -> renderTokenTemplate tokenTemplate $ context token' appHostName'
    getEmailDetails :: (WithMail env m) => env -> m (UserName, Text, Password)
    getEmailDetails config = do
      let gmail = getGoogleMail config
          pass = getGooglePass config
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
      "<form method=post action={{ appHostName }}" ++ "activate>"
        ++ "<input type=hidden name=token value={{ token }}>"
        ++ "<button type=submit\">Activate</button>"
        ++ "</form>"

context :: Text -> Text -> HashMap Text Text
context token host = HashMap.fromList [("token", token), ("appHostName", host)]

scopeLookup :: (Hashable k, Eq k, ToGVal m b) => k -> HashMap.HashMap k b -> GVal m
scopeLookup key context' = toGVal $ HashMap.lookup key context'

nullResolver :: IncludeResolver Identity
nullResolver = const $ return Nothing
