{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JWT where

import RIO hiding (catch)
import RIO.Time
import Web.Libjwt
import qualified Web.Libjwt as LJ
import Servant.Auth.Server (def)
import Control.Arrow (left)
import Control.Exception (catch, displayException)
import Data.Either.Validation (validationToEither)
import Model
import Config

makeToken :: User -> UTCTime -> ByteString
makeToken user = getToken . sign hmac512 <$> mkPayload user
  where
    mkPayload User {..} currentTime =
      let now = fromUTC currentTime
      in  def
            { iss           = Iss (Just "someapi")
            , aud           = Aud ["someapi"]
            , iat           = Iat (Just now)
            , LJ.exp        = Exp (Just $ now `plusSeconds` 7200)
            , privateClaims = toPrivateClaims
                                ( #userName ->> userName
                                , #userAge ->> userAge
                                , #userEmail ->> userEmail
                                , #userActivated ->> userActivated
                                )
            }

type UserJwt
  = Jwt '["userName" ->> Text, "userAge" ->> Maybe Int, "userEmail" ->> Text, "userActivated" ->> Maybe Bool] 'NoNs

instance ToPrivateClaims User
instance FromPrivateClaims User

decodeAndValidateFull :: ByteString -> IO (Either String User)
decodeAndValidateFull token = 
  (left (("Token not valid: " ++) . show) . fmap toUser . validationToEither <$> decodeAndValidate token) `catch` onError
 where
  toUser = fromPrivateClaims . privateClaims . payload . getValid
  onError (e :: SomeDecodeException) =
    return $ Left $ "Cannot decode token " ++ displayException e

decodeAndValidate :: ByteString -> IO (ValidationNEL ValidationFailure (Validated UserJwt))
decodeAndValidate = jwtFromByteString settings mempty hmac512
  where settings = Settings { leeway = 20, appName = Just "someapi" }

--

makeAuthToken :: Scope -> UTCTime -> ByteString
makeAuthToken scope = getToken . sign hmac512 <$> mkPayload scope
  where
    mkPayload Scope {..} currentTime =
      let now = fromUTC currentTime
      in  def
            { iss           = Iss (Just "someapi")
            , aud           = Aud ["someapi"]
            , iat           = Iat (Just now)
            , LJ.exp        = Exp (Just $ now `plusSeconds` 900)
            , privateClaims = toPrivateClaims
                                ( #protected ->> protected
                                , #private ->> private
                                )
            }

type AuthJwt = Jwt '["protected" ->> Bool, "private" ->> Bool] 'NoNs

instance ToPrivateClaims Scope
instance FromPrivateClaims Scope

decodeAndValidateFullAuth :: ByteString -> IO (Either String Scope)
decodeAndValidateFullAuth token =
  (left (("Invalid token: " ++) . show) . fmap toScope . validationToEither <$> decodeAndValidateAuth token) `catch` onError
 where
  toScope = fromPrivateClaims . privateClaims . payload . getValid
  onError (e :: SomeDecodeException) =
    return $ Left $ "Cannot decode token " ++ displayException e

decodeAndValidateAuth :: ByteString -> IO (ValidationNEL ValidationFailure (Validated AuthJwt))
decodeAndValidateAuth = jwtFromByteString settings mempty hmac512
  where settings = Settings { leeway = 5, appName = Just "someapi" }
