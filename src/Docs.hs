{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Docs where

import RIO
import RIO.Text (pack)
import Lens.Micro
import Servant.Multipart
import Database.Persist.Sql (Entity(..), toSqlKey)
import qualified Data.ByteString.Lazy.Char8 as LB
import Servant
import Servant.Swagger
import Data.Swagger
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson
import Model
import Api

writeSwaggerJSON :: IO ()
writeSwaggerJSON = LB.writeFile "swagger-docs/api.json" (encodePretty userSwagger)

type SwaggerAPI = "api.json" :> Get '[JSON] Swagger

userSwagger :: Swagger
userSwagger = toSwagger userApi
  & info.title   .~ "some API"
  & info.version .~ "1.0"
  & info.description ?~ "some api"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

instance ToSchema User where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User"
    & mapped.schema.example ?~ toJSON userSample

instance ToSchema UserWithPassword where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User with a password"
    & mapped.schema.example ?~ toJSON userWPSample

instance ToSchema Token where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "User token"
    & mapped.schema.example ?~ toJSON tokenSample

instance HasSwagger  (MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))) where
  toSwagger _ = mempty

instance ToSchema (Entity User) where
  declareNamedSchema proxy = pure $ NamedSchema Nothing mempty

instance ToSchema (Key User) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty

entityUserSample :: Entity User
entityUserSample = Entity (toSqlKey 1) (User "nini" (Just 100) "nini@mail.com" (Just True))

userSample :: User
userSample = User "nini" (Just 100) "nini@mail.com" (Just True)

userWPSample :: UserWithPassword
userWPSample = UserWithPassword "nini" (Just 100) "nini@mail.com" "pass"

tokenSample :: Token
tokenSample = Token "nini" token'

token' :: Text
token' = "eyjhbgcioijiuzuxmiisinr5cci6ikpxvcj9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
