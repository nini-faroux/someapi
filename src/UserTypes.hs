{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UserTypes 
  ( Age
  , Name
  , Email
  , VError(..)
  , makeName
  , makeAge
  , makeEmail
  , validActivation
  , validPassword
  , nameSample
  , ageSample
  , emailSample
  )
  where

import RIO ( Generic, encodeUtf8, Text )
import Data.Validation ( Validation(..) )
import qualified Data.Text as T
import qualified Text.Email.Validate as EV
import Data.Aeson (FromJSON, ToJSON)
import qualified Database.Persist.TH as PTH
import Libjwt.Classes ( JwtRep(..) )

newtype Name = Name { unName :: Text } deriving (Read, Generic)
newtype Age = Age { unAge :: Int } deriving (Read, Show, Generic)
newtype Email = Email { unEmail :: Text } deriving (Read, Generic)

instance FromJSON Name
instance ToJSON Name
instance FromJSON Age
instance ToJSON Age
instance FromJSON Email
instance ToJSON Email

instance Show Name where
  show (Name name) = show name
instance Show Email where
  show (Email email) = show email

instance JwtRep Text Name where
  rep = unName
  unRep = Just . Name

instance JwtRep Int Age where
  rep = unAge
  unRep = Just . Age

instance JwtRep Text Email where
  rep = unEmail
  unRep = Just . Email

PTH.derivePersistField "Name"
PTH.derivePersistField "Age"
PTH.derivePersistField "Email"

data VError =
    ExistingEmail
  | InvalidEmail
  | InvalidAge
  | InvalidName
  | InvalidPassword
  | InvalidActivation
  deriving Show

makeAge :: Int -> Validation [VError] Age
makeAge age
  | age >= 0 && age <= 120 = Success $ Age age
  | otherwise = Failure [InvalidAge]

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

-- | Export for the swagger docs
nameSample :: Name
nameSample = Name "nini"

ageSample :: Age
ageSample = Age 100

emailSample :: Email
emailSample = Email "nini@gmail.com"
