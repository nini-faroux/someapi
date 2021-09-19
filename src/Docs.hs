{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Docs where

import RIO
import Servant.Docs
import Servant.Multipart
import Model
import Api
import Database.Persist.Sql (Entity(..), toSqlKey)

apiDocs :: API
apiDocs = docs userApi

instance ToSample (Entity User) where
  toSamples _ = singleSample entityUserSample

instance ToSample User where
  toSamples _ = singleSample userSample

instance ToSample UserWithPassword where
  toSamples _ = singleSample userWPSample

instance ToSample Int64 where
  toSamples _ = singleSample 64

instance ToSample Token where
  toSamples _ = singleSample tokenSample

instance ToSample Text where
  toSamples _ = singleSample "some text"

instance ToMultipartSample Mem (MultipartData Mem) where
  toMultipartSamples proxy =
    [("sample 1"
     , MultipartData 
       [Input "token" token']
       [FileData "" "" "" ""]
     )
    ]

entityUserSample :: Entity User
entityUserSample = Entity (toSqlKey 1) (User "nini" (Just 100) "nini@mail.com" (Just True))

userSample :: User
userSample = User "nini" (Just 100) "nini@mail.com" (Just True)

userWPSample :: UserWithPassword
userWPSample = UserWithPassword "nini" (Just 100) "nini@mail.com" "pass"

tokenSample :: Token
tokenSample = Token "nini" token'

token' :: Text
token' = "eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJhdWQiOlsic29tZWFwaSJdLCJleHAiOjE2MzE5NjU3MDcsImlhdCI6MTYzMTk2NDgwNywiaXNzIjoic29tZWFwaSIsInByaXZhdGUiOmZhbHNlLCJwcm90ZWN0ZWQiOnRydWV9.CTEFPu36V0NEHRkWL_IV4rJ4J87CL1Irac0Mn99x6lRslYvXLVDaabyDkhV_QqyOeAtq95x4hIAeSJIhE03hT"
