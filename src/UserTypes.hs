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
  , renderName
  , renderEmail
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

newtype Name = Name Text deriving (Show, Read, Generic)
newtype Age = Age Int deriving (Show, Read, Generic)
newtype Email = Email Text deriving (Show, Read, Generic)

renderEmail :: Email -> Text
renderEmail (Email email) = email

renderName :: Name -> Text
renderName (Name name) = name

renderAge :: Age -> Int
renderAge (Age age) = age

instance FromJSON Name
instance ToJSON Name
instance FromJSON Age
instance ToJSON Age
instance FromJSON Email
instance ToJSON Email

instance JwtRep Text Name where
  rep = renderName
  unRep = Just . Name

instance JwtRep Int Age where
  rep = renderAge
  unRep = Just . Age

instance JwtRep Text Email where
  rep = renderEmail
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