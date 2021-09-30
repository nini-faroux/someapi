{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy, responseBody)
import qualified Database.Persist as P
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (Entity(..), toSqlKey)
import RIO (liftIO)
import Data.String (IsString)
import Servant
import Servant.Client
import Api
import Server (userServer, hoistAppServer)
import Model
import App
import UserTypes (nameSample, ageSample, emailSample)

main :: IO ()
main = hspec apiTests

apiTests :: Spec
apiTests =
  around withUserApp $ do
    let createUser = client (Proxy :: Proxy CreateUser)
    let getUser = client (Proxy :: Proxy GetUser)
    let getUsers = client (Proxy :: Proxy GetUsers)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = 8000 })

    describe "GET /users" $
      it "should return empty list of users" $ \_port -> do
        result <- runClientM getUsers clientEnv
        result `shouldBe` Right []

    describe "POST /user" $
      it "should not create invalid user, and should report all validation errors" $ \_port -> do
        result <- runClientM (createUser malformedUser) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[InvalidName,InvalidAge,InvalidEmail,InvalidPassword]"
          Right res -> liftIO $ print res

    describe "POST /user" $
      it "should create a valid user" $ \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right 1

    describe "POST /user" $
      it "should fail and report that email exists" $ \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[ExistingEmail]"
          Right res -> liftIO $ print res

    describe "GET /users" $
      it "should return list of users" $ \_port -> do
        result <- runClientM getUsers clientEnv
        result `shouldBe` Right [createdEntityUserFromSample1]

    describe "GET /user/1" $
      it "should get valid user" $ \_port -> do
        result <- runClientM (getUser (toSqlKey 1)) clientEnv
        case result of
          Left err -> liftIO $ print err
          Right result -> liftIO $ print result
        result `shouldBe` Right createdEntityUserFromSample1

    describe "GET /user/300" $
      it "should fail with user not found" $ \_port -> do
        result <- runClientM (getUser (toSqlKey 300)) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "User not found"

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp = testWithApplication userApp

userApp :: IO Application
userApp = serve userApi <$> userServer'

userServer' :: IO (Server UserAPI)
userServer' = initialEnv >>= \env -> pure $ hoistAppServer env 

userWPSample1 :: UserWithPassword
userWPSample1 = UserWithPassword "nini" 100 "nini@mail.com" "password"

malformedUser :: UserWithPassword
malformedUser = UserWithPassword "lo" 130 "lou@@mail.com" "pass"

createdEntityUserFromSample1 :: Entity User
createdEntityUserFromSample1 = Entity (toSqlKey 1) createdUserFromSample1

createdUserFromSample1 :: User
createdUserFromSample1 = User nameSample ageSample emailSample (Just False)

userWPSample :: UserWithPassword
userWPSample = UserWithPassword "nini" 100 "nini@mail.com" "password"
