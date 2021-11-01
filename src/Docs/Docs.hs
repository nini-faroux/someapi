{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs.Docs (
  writeSwaggerJSON,
) where

import Api (noteApi)
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Fixed (Pico)
import Data.Swagger (
  NamedSchema (..),
  Swagger (..),
  ToParamSchema (..),
  ToSchema (..),
  URL (..),
  declareNamedSchema,
  defaultSchemaOptions,
  description,
  example,
  genericDeclareNamedSchema,
  info,
  license,
  schema,
  title,
  toParamSchema,
  url,
  version,
 )
import Database.Persist.Sql (Entity (..))
import Lens.Micro (mapped, (?~))
import Parse.NoteTypes (
  Date,
  DateField,
  DateInput (..),
  Month,
  NoteBody,
  NoteTitle,
  Year,
  dateSample,
  noteBodySample,
  noteTitleSample,
 )
import Parse.UserTypes (
  Email,
  Name,
  emailSample,
  nameSample,
 )
import RIO
import RIO.Time (
  TimeOfDay (TimeOfDay),
  UTCTime (UTCTime),
  fromGregorian,
  timeOfDayToTime,
 )
import Servant
import Servant.Multipart (
  Mem,
  MultipartData,
  MultipartForm,
 )
import Servant.Swagger
import Web.JWT (
  Token (..),
 )
import Web.Model (
  Key (..),
  Note (..),
  NoteInput (..),
  User (..),
  UserLogin (..),
  UserWithPassword (..),
 )

writeSwaggerJSON :: IO ()
writeSwaggerJSON = LB.writeFile "swagger-docs/api.json" (encodePretty userSwagger)

userSwagger :: Swagger
userSwagger =
  toSwagger noteApi
    & info . title .~ "some API"
    & info . version .~ "1.0"
    & info . description ?~ "some api"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "User"
      & mapped . schema . example ?~ toJSON userSample

instance ToSchema Note where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Note"
      & mapped . schema . example ?~ toJSON noteSample

instance ToSchema NoteInput where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "NoteInput"
      & mapped . schema . example ?~ toJSON noteInputSample

instance ToSchema NoteBody where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "NoteBody"
      & mapped . schema . example ?~ toJSON noteBodySample

instance ToSchema NoteTitle where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "NoteTitle"
      & mapped . schema . example ?~ toJSON noteTitleSample

instance ToSchema Date where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Date"
      & mapped . schema . example ?~ toJSON dateSample

instance ToSchema DateField where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema Month where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema Year where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

instance ToSchema DateInput where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "DateInput"
      & mapped . schema . example ?~ toJSON dateInputSample

instance ToSchema Email where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Email"
      & mapped . schema . example ?~ toJSON emailSample

instance ToSchema Name where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "Name"
      & mapped . schema . example ?~ toJSON nameSample

instance ToSchema UserWithPassword where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "User with a password"
      & mapped . schema . example ?~ toJSON userWPSample

instance ToSchema UserLogin where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "User login"
      & mapped . schema . example ?~ toJSON userLoginSample

instance ToSchema Token where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "User token"
      & mapped . schema . example ?~ toJSON tokenSample

instance HasSwagger (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))) where
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

makeUTCTime ::
  (Integer, Int, Int) ->
  (Int, Int, Pico) ->
  UTCTime
makeUTCTime (year, mon, date) (hour, minute, sec) =
  UTCTime
    (fromGregorian year mon date)
    (timeOfDayToTime (TimeOfDay hour minute sec))

userSample :: User
userSample = User nameSample emailSample (Just True)

noteSample :: Note
noteSample = Note nameSample noteTitleSample noteBodySample (makeUTCTime (2021, 9, 30) (20, 42, 0)) dateSample

noteInputSample :: NoteInput
noteInputSample = NoteInput "nini" "some name" "do something good"

dateInputSample :: DateInput
dateInputSample = DateInput 2021 10 5

userWPSample :: UserWithPassword
userWPSample = UserWithPassword "nini" "nini@mail.com" "password"

userLoginSample :: UserLogin
userLoginSample = UserLogin "nini@mail.com" "password"

tokenSample :: Token
tokenSample = Token token'

token' :: Text
token' = "eyjhbgcioijiuzuxmiisinr5cci6ikpxvcj9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
