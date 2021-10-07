{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs (writeSwaggerJSON) where

import Data.Swagger 
  ( ToSchema(..)
  , ToParamSchema(..)
  , Swagger(..)
  , NamedSchema(..)
  , URL(..)
  , example
  , genericDeclareNamedSchema
  , defaultSchemaOptions
  , description
  , declareNamedSchema
  , toParamSchema
  , schema
  , info
  , license
  , version
  , title
  , url
  )
import Servant
import Servant.Swagger
import Servant.Multipart (MultipartForm, MultipartData, Mem)
import RIO.Text (Text)
import RIO.Time (UTCTime(UTCTime), TimeOfDay(TimeOfDay), fromGregorian, timeOfDayToTime)
import Data.Fixed (Pico)
import Lens.Micro (mapped, (&), (?~), (.~))
import Database.Persist.Sql (Entity(..))
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (toJSON)
import Model (User(..), UserWithPassword(..), UserLogin(..), NoteInput(..), Key(..), Note(..))
import JWT (Token(..))
import Api (noteApi)
import UserTypes (Name, Age, Email, nameSample, ageSample, emailSample)
import NoteTypes (NoteTitle, NoteBody, Day, DayField, Month, Year, DayInput(..), noteTitleSample, noteBodySample, daySample)

writeSwaggerJSON :: IO ()
writeSwaggerJSON = LB.writeFile "swagger-docs/api.json" (encodePretty userSwagger)

userSwagger :: Swagger
userSwagger = toSwagger noteApi
  & info.title   .~ "some API"
  & info.version .~ "1.0"
  & info.description ?~ "some api"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User"
    & mapped.schema.example ?~ toJSON userSample

instance ToSchema Note where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Note"
    & mapped.schema.example ?~ toJSON noteSample

instance ToSchema NoteInput where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NoteInput"
    & mapped.schema.example ?~ toJSON noteInputSample

instance ToSchema NoteBody where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NoteBody"
    & mapped.schema.example ?~ toJSON noteBodySample

instance ToSchema NoteTitle where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NoteTitle"
    & mapped.schema.example ?~ toJSON noteTitleSample

instance ToSchema Day where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Day"
    & mapped.schema.example ?~ toJSON daySample

instance ToSchema DayField where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema Month where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema Year where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema DayInput where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "DayInput"
    & mapped.schema.example ?~ toJSON dayInputSample

instance ToSchema Email where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Email"
    & mapped.schema.example ?~ toJSON emailSample

instance ToSchema Age where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Age"
    & mapped.schema.example ?~ toJSON ageSample

instance ToSchema Name where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Name"
    & mapped.schema.example ?~ toJSON nameSample

instance ToSchema UserWithPassword where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User with a password"
    & mapped.schema.example ?~ toJSON userWPSample

instance ToSchema UserLogin where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User login"
    & mapped.schema.example ?~ toJSON userLoginSample

instance ToSchema Token where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User token"
    & mapped.schema.example ?~ toJSON tokenSample

instance HasSwagger  (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))) where
  toSwagger _ = mempty

instance ToSchema (Entity User) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema (Entity Note) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema (Key User) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema (Key Note) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToParamSchema Token where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

instance ToParamSchema (Key User) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)

makeUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
makeUTCTime (year, mon, day) (hour, minute, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour minute sec))

userSample :: User
userSample = User nameSample ageSample emailSample (Just True)

noteSample :: Note
noteSample = Note nameSample noteTitleSample noteBodySample (makeUTCTime (2021, 9, 30) (20, 42, 0)) daySample

noteInputSample :: NoteInput
noteInputSample = NoteInput "nini" "some name" "do something good"

dayInputSample :: DayInput
dayInputSample = DayInput 2021 10 5

userWPSample :: UserWithPassword
userWPSample = UserWithPassword "nini" 100 "nini@mail.com" "password"

userLoginSample :: UserLogin
userLoginSample = UserLogin "nini@mail.com" "password"

tokenSample :: Token
tokenSample = Token token'

token' :: Text
token' = "eyjhbgcioijiuzuxmiisinr5cci6ikpxvcj9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
