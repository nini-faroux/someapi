{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parse.UserTypes 
  ( Name
  , Email
  , makeName
  , makeEmail
  , validActivation
  , validPassword
  , renderName
  , renderEmail
  , nameSample
  , nameSample2
  , emailSample
  )
  where

import RIO
import Data.Validation ( Validation(..) )
import qualified Data.Text as T
import qualified Text.Email.Validate as EV
import Data.Aeson (FromJSON, ToJSON)
import qualified Database.Persist.TH as PTH
import Libjwt.Classes ( JwtRep(..) )
import Parse.Validation (VError(..))

newtype Name = Name Text deriving (Eq, Show, Read, Generic)
newtype Email = Email Text deriving (Eq, Show, Read, Generic)

renderEmail :: Email -> Text
renderEmail (Email email) = email

renderName :: Name -> Text
renderName (Name name) = name

instance FromJSON Name
instance ToJSON Name
instance FromJSON Email
instance ToJSON Email

instance JwtRep Text Name where
  rep = renderName
  unRep = Just . Name

instance JwtRep Text Email where
  rep = renderEmail
  unRep = Just . Email

PTH.derivePersistField "Name"
PTH.derivePersistField "Email"

makeName :: Text -> Validation [VError] Name
makeName name
  | nameLength < 4 || nameLength > 20 = Failure [InvalidName] 
  | otherwise = Success $ Name name
  where nameLength = T.length name

makeEmail :: Text -> Validation [VError] Email
makeEmail email
  | EV.isValid (encodeUtf8 email) = Success $ Email email
  | otherwise = Failure [InvalidEmail]

validActivation :: Maybe Bool -> Validation [VError] (Maybe Bool)
validActivation Nothing = Success Nothing
validActivation (Just activation)
  | activation = Failure [InvalidActivation]
  | otherwise = Success (Just False)

validPassword :: Text -> Validation [VError] Text
validPassword pass
  | lengthPass < 8 = Failure [InvalidPassword]
  | otherwise = Success pass
  where lengthPass = T.length pass

-- | Export for the swagger docs and tests
nameSample :: Name
nameSample = Name "nini"

nameSample2 :: Name
nameSample2 = Name "laurie"

emailSample :: Email
emailSample = Email "nini@mail.com"
