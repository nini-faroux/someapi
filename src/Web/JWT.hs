{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.JWT (
  WithAuthToken (..),
  Scope (..),
  Token,
  WithUserToken (..),
  tokenSample,
) where

import App (App, HasSecret (..), WithTime (..))
import Control.Arrow (left)
import Control.Monad.Time (MonadTime)
import Data.Aeson (
  FromJSON,
  ToJSON,
 )
import qualified Data.ByteString.Char8 as LC
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Either.Validation (validationToEither)
import qualified Data.Text as T
import Parse.UserTypes (
  Email,
  Name,
 )
import Parse.Validation (WithError (..))
import RIO
import RIO.Time (
  NominalDiffTime,
  UTCTime,
 )
import Servant (
  err400,
  errBody,
 )
import Servant.Auth.Server (def)
import System.Environment (getEnv)
import Web.HttpApiData (
  FromHttpApiData,
  ToHttpApiData,
 )
import Web.Libjwt
import qualified Web.Libjwt as LJ
import Web.Model (User (..))

-- Type for the private claims of the JWT token
data Scope = Scope {protectedAccess :: Bool, tokenUserName :: Name}
  deriving stock (Generic)

newtype Token = Token {token :: Text}
  deriving (Generic, FromHttpApiData, ToHttpApiData)

type UserJwt =
  Jwt
    '[ "userName" ->> Name
     , "userEmail" ->> Email
     , "userActivated" ->> Maybe Bool
     ]
    'NoNs

type AuthJwt =
  Jwt
    '[ "protectedAccess" ->> Bool
     , "tokenUserName" ->> Name
     ]
    'NoNs

instance ToPrivateClaims User

instance FromPrivateClaims User

instance ToPrivateClaims Scope

instance FromPrivateClaims Scope

instance FromJSON Token

instance ToJSON Token

class Monad m => WithUserToken m where
  makeUserToken :: User -> UTCTime -> m ByteString
  verifyUserToken :: Text -> m User

instance WithUserToken App where
  makeUserToken user time = do
    secret <- asks getHmacSecret
    liftIO $ makeUserToken' user time secret
    where
      makeUserToken' :: User -> UTCTime -> String -> IO ByteString
      makeUserToken' User {..} = makeToken claims 7200
        where
          claims =
            ( #userName ->> userName
            , #userEmail ->> userEmail
            , #userActivated ->> userActivated
            )

  verifyUserToken token = do
    secret <- asks getHmacSecret
    eUser <- liftIO $ decodeAndValidateUser (encodeUtf8 token) secret
    case eUser of
      Left err -> throwError err400 {errBody = LB.fromString err}
      Right user -> return user

class Monad m => WithAuthToken m where
  verifyAuthToken :: Maybe Token -> m Scope
  makeAuthToken :: Name -> m Token

instance WithAuthToken App where
  verifyAuthToken Nothing = throwError err400 {errBody = "Token Missing"}
  verifyAuthToken (Just (Token token)) = do
    secret <- asks getHmacSecret
    eScope <- decodeToken token secret
    case eScope of
      Left err -> throwError err400 {errBody = LB.fromString err}
      Right scope -> return scope
    where
      -- Need to trim the token to account for the 'Bearer' prefix
      -- otherwise in that case it will raise a decoding error
      decodeToken token' secret'
        | hasBearerPrefix token' =
          liftIO $
            decodeAndValidateAuth
              (encodeUtf8 $ T.init $ T.drop 8 token')
              secret'
        | otherwise =
          liftIO $
            decodeAndValidateAuth
              (encodeUtf8 token')
              secret'
      hasBearerPrefix token' = T.take 6 token' == "Bearer"

  makeAuthToken existingName = do
    secret <- asks getHmacSecret
    now <- getTime
    token <- liftIO $ makeToken' scope now secret
    case decodeUtf8' token of
      Left err -> throwError err400 {errBody = LB.fromString $ show err}
      Right token' -> return $ Token token'
    where
      scope = Scope {protectedAccess = True, tokenUserName = existingName}

makeToken' :: Scope -> UTCTime -> String -> IO ByteString
makeToken' Scope {..} = makeToken claims 900
  where
    claims =
      (#protectedAccess ->> protectedAccess, #tokenUserName ->> tokenUserName)

makeToken ::
  (Encode (PrivateClaims (Claims a) (OutNs a)), ToPrivateClaims a) =>
  a ->
  NominalDiffTime ->
  UTCTime ->
  String ->
  IO ByteString
makeToken privateClaims' seconds currTime hmacSecret = do
  let hmacSecret' = hmac512 hmacSecret
  return . getToken $ sign hmacSecret' $ makePayload currTime
  where
    makePayload currTime' =
      let now = fromUTC currTime'
       in def
            { iss = Iss (Just "someapi")
            , aud = Aud ["someapi"]
            , iat = Iat (Just now)
            , LJ.exp = Exp (Just $ now `plusSeconds` seconds)
            , privateClaims = toPrivateClaims privateClaims'
            }

hmac512 :: String -> Algorithm Secret
hmac512 secret = HMAC512 $ MkSecret $ LC.pack secret

decodeAndValidateUser :: ByteString -> String -> IO (Either String User)
decodeAndValidateUser = decodeAndValidateFull decodeAndValidateUser'

decodeAndValidateAuth :: ByteString -> String -> IO (Either String Scope)
decodeAndValidateAuth = decodeAndValidateFull decodeAndValidateAuth'

decodeAndValidateUser' ::
  ByteString ->
  String ->
  IO (ValidationNEL ValidationFailure (Validated UserJwt))
decodeAndValidateUser' = decodeAndValidate

decodeAndValidateAuth' ::
  ByteString ->
  String ->
  IO (ValidationNEL ValidationFailure (Validated AuthJwt))
decodeAndValidateAuth' = decodeAndValidate

decodeAndValidateFull ::
  (Show e, FromPrivateClaims b) =>
  ( ByteString ->
    String ->
    IO (ValidationNEL e (Validated (Jwt (Claims b) namespace)))
  ) ->
  ByteString ->
  String ->
  IO (Either String b)
decodeAndValidateFull decode token secret =
  ( left (("Token not valid: " ++) . show)
      . fmap toInnerType
      . validationToEither
      <$> decode token secret
  )
    `catch` onError
  where
    toInnerType = fromPrivateClaims . privateClaims . payload . getValid
    onError (e :: SomeDecodeException) =
      return $ Left $ "Cannot decode token " ++ displayException e

decodeAndValidate ::
  (Decode (PrivateClaims a b), MonadTime m, MonadThrow m) =>
  ByteString ->
  String ->
  m (ValidationNEL ValidationFailure (Validated (Jwt a b)))
decodeAndValidate token secret = do
  let hmac512' = hmac512 secret
  jwtFromByteString settings mempty hmac512' token
  where
    settings = Settings {leeway = 5, appName = Just "someapi"}

-- | For docs and tests

-- | Need an IO instance for testing to simulate when a user has authenticated
instance WithAuthToken IO where
  verifyAuthToken Nothing = throwError err400 {errBody = "Token Missing"}
  verifyAuthToken (Just (Token token)) = do
    secret <- getEnv "HMAC_SECRET"
    eScope <- decodeToken token secret
    case eScope of
      Left err -> throwError err400 {errBody = LB.fromString err}
      Right scope -> return scope
    where
      decodeToken token'
        | hasBearerPrefix token' =
          decodeAndValidateAuth $ encodeUtf8 $ T.init $ T.drop 8 token'
        | otherwise = decodeAndValidateAuth $ encodeUtf8 token'
      hasBearerPrefix token' = T.take 6 token' == "Bearer"

  makeAuthToken existingName = do
    now <- getTime
    secret <- getEnv "HMAC_SECRET"
    token <- makeToken' scope now secret
    case decodeUtf8' token of
      Left err -> throwError err400 {errBody = LB.fromString $ show err}
      Right token' -> return $ Token token'
    where
      scope = Scope {protectedAccess = True, tokenUserName = existingName}

{- | The following sample has to be in this module, as the Token
type is abstract, so the Token constructor is only accessible in this module
-}
tokenSample :: Token
tokenSample = Token tokenText

tokenText :: Text
tokenText = "eyjhbgcioijiuzuxmiisinr5cci6ikpxvcj9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
