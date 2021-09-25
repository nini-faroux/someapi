{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module JWT 
  ( makeAuthToken
  , makeUserToken
  , decodeAndValidateUser
  , decodeAndValidateAuth
  ) where

import Web.Libjwt
import RIO hiding (catch)
import RIO.Time (UTCTime, NominalDiffTime)
import qualified Web.Libjwt as LJ
import Servant.Auth.Server (def)
import Control.Arrow (left)
import Control.Exception (catch, displayException)
import Data.Either.Validation (validationToEither)
import Control.Monad.Time (MonadTime)
import Model (User(..), Scope(..))
import Config (hmac512)
import UserTypes (Name, Age, Email)

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
  where claims = (#protectedAccess ->> protectedAccess, #privateAccess ->> privateAccess)

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

type AuthJwt = Jwt '["protectedAccess" ->> Bool, "privateAccess" ->> Bool] 'NoNs

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
