{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.JWT 
  ( Scope(..)
  , Token(..)
  , makeAuthToken
  , makeUserToken
  , verifyAuthToken
  , verifyUserToken
  ) where

import RIO
import Web.Libjwt
import Servant (errBody, err400)
import RIO.Time (UTCTime, NominalDiffTime)
import qualified Web.Libjwt as LJ
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.ByteString.Char8 as LC
import qualified Data.Text as T
import Servant.Auth.Server (def)
import Control.Arrow (left)
import Data.Either.Validation (validationToEither)
import Control.Monad.Time (MonadTime)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Data.Aeson (FromJSON, ToJSON)
import System.Environment (getEnv)
import Web.Model (User(..))
import Parse.UserTypes (Name, Age, Email)
import App (App)

-- Type for the private claims of the JWT token
data Scope = Scope { protectedAccess :: Bool, tokenUserName :: Name }
  deriving stock (Show, Eq, Generic)

newtype Token = Token { token :: Text }
  deriving (Eq, Show, Generic, FromHttpApiData, ToHttpApiData)

type UserJwt
  = Jwt '["userName" ->> Name, "userAge" ->> Age, "userEmail" ->> Email, "userActivated" ->> Maybe Bool] 'NoNs

type AuthJwt = Jwt '["protectedAccess" ->> Bool, "tokenUserName" ->> Name] 'NoNs

instance ToPrivateClaims User
instance FromPrivateClaims User

instance ToPrivateClaims Scope
instance FromPrivateClaims Scope

instance FromJSON Token
instance ToJSON Token

verifyUserToken :: Text -> App User
verifyUserToken token = do
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right user -> return user

verifyAuthToken :: Maybe Token -> App Scope
verifyAuthToken Nothing = throwIO err400 { errBody = "Token Missing" }
verifyAuthToken (Just (Token token)) = do
  eScope <- decodeToken token
  case eScope of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right scope -> return scope
  where
    -- Need to trim the token to account for the 'Bearer' prefix 
    -- otherwise in that case it will raise a decoding error
    decodeToken token'
          | hasBearerPrefix token' = liftIO . decodeAndValidateAuth $ encodeUtf8 $ T.init $ T.drop 8 token'
          | otherwise = liftIO . decodeAndValidateAuth $ encodeUtf8 token'
    hasBearerPrefix token' = T.take 6 token' == "Bearer"

makeUserToken :: User -> UTCTime -> IO ByteString
makeUserToken User {..} = makeToken claims 7200
  where
    claims = ( #userName ->> userName
             , #userAge ->> userAge
             , #userEmail ->> userEmail
             , #userActivated ->> userActivated
             )

makeAuthToken :: Scope -> UTCTime -> IO ByteString
makeAuthToken Scope {..} = makeToken claims 900
  where claims = (#protectedAccess ->> protectedAccess, #tokenUserName ->> tokenUserName)

makeToken :: (Encode (PrivateClaims (Claims a) (OutNs a)), ToPrivateClaims a) =>
  a -> NominalDiffTime -> UTCTime -> IO ByteString
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

decodeAndValidateFull
  :: (Show e, FromPrivateClaims b) =>
     (ByteString -> IO (ValidationNEL e (Validated (Jwt (Claims b) namespace))))
  -> ByteString
  -> IO (Either String b)
decodeAndValidateFull decode token = 
  (left (("Token not valid: " ++) . show) . fmap toInnerType . validationToEither <$> decode token) `catch` onError
 where
  toInnerType = fromPrivateClaims . privateClaims . payload . getValid
  onError (e :: SomeDecodeException) =
    return $ Left $ "Cannot decode token " ++ displayException e

decodeAndValidate :: (Decode (PrivateClaims a b), MonadTime m, MonadThrow m, MonadIO m) => 
  ByteString -> m (ValidationNEL ValidationFailure (Validated (Jwt a b)))
decodeAndValidate token = do
  hmac512' <- liftIO hmac512
  jwtFromByteString settings mempty hmac512' token
  where
    settings = Settings { leeway = 5, appName = Just "someapi" }
