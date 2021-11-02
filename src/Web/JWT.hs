{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.JWT (
  AuthToken (..),
  Scope (..),
  Token,
  UserToken (..),
  makeAuthToken',
  tokenSample,
) where

import App (App, WithTime (..))
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
import Parse.Validation (Error (..))
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
  Jwt '["userName" ->> Name, "userEmail" ->> Email, "userActivated" ->> Maybe Bool] 'NoNs

type AuthJwt = Jwt '["protectedAccess" ->> Bool, "tokenUserName" ->> Name] 'NoNs

instance ToPrivateClaims User

instance FromPrivateClaims User

instance ToPrivateClaims Scope

instance FromPrivateClaims Scope

instance FromJSON Token

instance ToJSON Token

class Monad m => UserToken m where
  makeUserToken :: User -> UTCTime -> m ByteString
  verifyUserToken :: Text -> m User

instance UserToken App where
  makeUserToken user time = liftIO $ makeUserToken' user time
    where
      makeUserToken' :: User -> UTCTime -> IO ByteString
      makeUserToken' User {..} = makeToken claims 7200
        where
          claims =
            ( #userName ->> userName
            , #userEmail ->> userEmail
            , #userActivated ->> userActivated
            )

  verifyUserToken token = do
    eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
    case eUser of
      Left err -> throwError err400 {errBody = LB.fromString err}
      Right user -> return user

class Monad m => AuthToken m where
  verifyAuthToken :: Maybe Token -> m Scope
  makeAuthToken :: Scope -> UTCTime -> m ByteString

instance AuthToken App where
  verifyAuthToken Nothing = throwError err400 {errBody = "Token Missing"}
  verifyAuthToken (Just (Token token)) = do
    eScope <- decodeToken token
    case eScope of
      Left err -> throwError err400 {errBody = LB.fromString err}
      Right scope -> return scope
    where
      -- Need to trim the token to account for the 'Bearer' prefix
      -- otherwise in that case it will raise a decoding error
      decodeToken token'
        | hasBearerPrefix token' = liftIO . decodeAndValidateAuth $ encodeUtf8 $ T.init $ T.drop 8 token'
        | otherwise = liftIO . decodeAndValidateAuth $ encodeUtf8 token'
      hasBearerPrefix token' = T.take 6 token' == "Bearer"

  makeAuthToken scope time = liftIO $ makeToken' scope time
    where
      makeToken' :: Scope -> UTCTime -> IO ByteString
      makeToken' Scope {..} = makeToken claims 900
        where
          claims = (#protectedAccess ->> protectedAccess, #tokenUserName ->> tokenUserName)

makeAuthToken' ::
  ( AuthToken m
  , Error m
  , WithTime m
  ) =>
  Name ->
  m Token
makeAuthToken' existingName = do
  now <- getTime
  token <- makeAuthToken (Scope {protectedAccess = True, tokenUserName = existingName}) now
  case decodeUtf8' token of
    Left err -> throwError err400 {errBody = LB.fromString $ show err}
    Right token' -> return $ Token token'

makeToken ::
  (Encode (PrivateClaims (Claims a) (OutNs a)), ToPrivateClaims a) =>
  a ->
  NominalDiffTime ->
  UTCTime ->
  IO ByteString
makeToken privateClaims' seconds currTime = do
  hmac512' <- hmac512
  return . getToken $ sign hmac512' $ makePayload currTime
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

hmac512 :: IO (Algorithm Secret)
hmac512 = do
  secret <- getEnv "HMAC_SECRET"
  return $ HMAC512 $ MkSecret $ LC.pack secret

decodeAndValidateUser :: ByteString -> IO (Either String User)
decodeAndValidateUser = decodeAndValidateFull decodeAndValidateUser'

decodeAndValidateAuth :: ByteString -> IO (Either String Scope)
decodeAndValidateAuth = decodeAndValidateFull decodeAndValidateAuth'

decodeAndValidateUser' :: ByteString -> IO (ValidationNEL ValidationFailure (Validated UserJwt))
decodeAndValidateUser' = decodeAndValidate

decodeAndValidateAuth' :: ByteString -> IO (ValidationNEL ValidationFailure (Validated AuthJwt))
decodeAndValidateAuth' = decodeAndValidate

decodeAndValidateFull ::
  (Show e, FromPrivateClaims b) =>
  (ByteString -> IO (ValidationNEL e (Validated (Jwt (Claims b) namespace)))) ->
  ByteString ->
  IO (Either String b)
decodeAndValidateFull decode token =
  (left (("Token not valid: " ++) . show) . fmap toInnerType . validationToEither <$> decode token) `catch` onError
  where
    toInnerType = fromPrivateClaims . privateClaims . payload . getValid
    onError (e :: SomeDecodeException) =
      return $ Left $ "Cannot decode token " ++ displayException e

decodeAndValidate ::
  (Decode (PrivateClaims a b), MonadTime m, MonadThrow m, MonadIO m) =>
  ByteString ->
  m (ValidationNEL ValidationFailure (Validated (Jwt a b)))
decodeAndValidate token = do
  hmac512' <- liftIO hmac512
  jwtFromByteString settings mempty hmac512' token
  where
    settings = Settings {leeway = 5, appName = Just "someapi"}

-- | Export for docs
tokenSample :: Token
tokenSample = Token tokenText

tokenText :: Text
tokenText = "eyjhbgcioijiuzuxmiisinr5cci6ikpxvcj9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
