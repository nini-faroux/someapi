{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import RIO.Time (getCurrentTime)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy, responseBody)
import qualified Database.Persist as P
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (Entity(..), toSqlKey)
import RIO (liftIO, decodeUtf8')
import Data.String (IsString)
import Servant
import Servant.Client
import Api
import Server (noteServer, hoistAppServer)
import Model
import App
import UserTypes (nameSample, ageSample, emailSample)
import NoteTypes (DayInput(..), validDay)
import Data.Validation
import JWT (makeAuthToken)

main :: IO ()
main = hspec apiTests

apiTests :: Spec
apiTests =
  around withUserApp $ do
    let createUser = client (Proxy :: Proxy CreateUser)
    let createNote = client (Proxy :: Proxy CreateNote)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = 8000 })

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
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right 1

    describe "POST /user" $
      it "should fail and report that email exists" $ \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[ExistingEmail,ExistingUserName]"
          Right res -> liftIO $ print res

    describe "POST /note" $
      it "should successfully create a new note" $ \_port -> do
        now <- getCurrentTime
        let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) now
        case decodeUtf8' token of
          Left _ -> liftIO $ print "couldn't decode token"
          Right token' -> do
            let note = NoteInput  "nini" "some note" "do something good"
            result <- runClientM (createNote note (Just $ Token token')) clientEnv
            case result of
              Left (FailureResponse _ response) ->
                liftIO $ print $ responseBody response
              Right res -> liftIO $ print res
            result `shouldBe` Right (toSqlKey 1)

    describe "POST /note" $
     it "should fail with wrong user name" $ \_port -> do
       now <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) now
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           let note = NoteInput  "major" "some note" "do something else"
           result <- runClientM (createNote note (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) -> do
               liftIO $ print $ responseBody response
               responseBody response `shouldBe` "Not Authorised - use your own user name to create new notes" 
             Right res -> liftIO $ print res

    describe "compare days" $
      it "should compare day types correctly" $ \_ -> do
        let day1 = DayInput 2021 3 5
            day2 = DayInput 2021 10 6
        case validDay day1 of
          Failure _ -> liftIO $ print "error valid day"
          Success day1' -> case validDay day2 of
            Failure _ -> liftIO $ print "error valid day"
            Success day2' -> day2' >= day1' `shouldBe` True

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp = testWithApplication noteApp

noteApp :: IO Application
noteApp = serve noteApi <$> noteServer'

noteServer' :: IO (Server NoteAPI)
noteServer' = initialEnv >>= \env -> pure $ hoistAppServer env 

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
