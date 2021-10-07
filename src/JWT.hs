{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JWT 
  ( makeAuthToken
  , makeUserToken
  , verifyAuthToken
  , verifyUserToken
  ) where

import Web.Libjwt
import Servant (errBody, err400)
import RIO (Text, ByteString, MonadThrow, liftIO, encodeUtf8, displayException, throwIO)
import RIO.Time (UTCTime, NominalDiffTime)
import qualified Web.Libjwt as LJ
import qualified Data.ByteString.Lazy.UTF8 as LB
import qualified Data.Text as T
import Servant.Auth.Server (def)
import Control.Arrow (left)
import Control.Exception (catch)
import Data.Either.Validation (validationToEither)
import Control.Monad.Time (MonadTime)
import Model (User(..), Scope(..), Token(..))
import Config (hmac512)
import UserTypes (Name, Age, Email)
import App (App)

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

makeUserToken :: User -> UTCTime -> ByteString
makeUserToken User {..} = makeToken claims 7200
  where
    claims = ( #userName ->> userName
             , #userAge ->> userAge
             , #userEmail ->> userEmail
             , #userActivated ->> userActivated
             )

makeAuthToken :: Scope -> UTCTime -> ByteString
makeAuthToken Scope {..} = makeToken claims 900
  where claims = (#protectedAccess ->> protectedAccess, #tokenUserName ->> tokenUserName)

makeToken :: (Encode (PrivateClaims (Claims a) (OutNs a)), ToPrivateClaims a) =>
  a -> NominalDiffTime -> UTCTime -> ByteString
makeToken privateClaims' seconds = getToken . sign hmac512 <$> mkPayload
  where
    mkPayload currentTime =
      let now = fromUTC currentTime
      in def
        { iss = Iss (Just "someapi")
        , aud = Aud ["someapi"]
        , iat = Iat (Just now)
        , LJ.exp = Exp (Just $ now `plusSeconds` seconds)
        , privateClaims = toPrivateClaims privateClaims'
        }

type UserJwt
  = Jwt '["userName" ->> Name, "userAge" ->> Age, "userEmail" ->> Email, "userActivated" ->> Maybe Bool] 'NoNs

type AuthJwt = Jwt '["protectedAccess" ->> Bool, "tokenUserName" ->> Name] 'NoNs

instance ToPrivateClaims User
instance FromPrivateClaims User

instance ToPrivateClaims Scope
instance FromPrivateClaims Scope

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

decodeAndValidate :: (Decode (PrivateClaims a b), MonadTime m, MonadThrow m) => 
  ByteString -> m (ValidationNEL ValidationFailure (Validated (Jwt a b)))
decodeAndValidate = jwtFromByteString settings mempty hmac512
  where settings = Settings { leeway = 5, appName = Just "someapi" }
