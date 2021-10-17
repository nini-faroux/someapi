{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import RIO.Time (UTCTime(UTCTime), TimeOfDay(TimeOfDay), getCurrentTime, fromGregorian, timeOfDayToTime)
import Data.Fixed (Pico)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.Wai.Handler.Warp
import Network.HTTP.Client hiding (Proxy, responseBody)
import qualified Database.Persist as P
import Data.Aeson (object, (.=), encode)
import Database.Persist.Sql (Entity(..), BackendKey(SqlBackendKey), toSqlKey)
import qualified Data.ByteString.Lazy.UTF8 as LB
import RIO (liftIO, decodeUtf8')
import Data.String (IsString)
import Servant
import Servant.Client
import Api
import Server (noteServer, hoistAppServer)
import Model
import App
import UserTypes (nameSample, nameSample2, ageSample, emailSample)
import NoteTypes (DayInput(..), validDay, validDayText)
import Data.Validation
import JWT (Scope(..), Token(..), makeAuthToken)

main :: IO ()
main = hspec apiTests

-- | Current tests assume starting with fresh database
apiTests :: Spec
apiTests =
  around withUserApp $ do
    let createUser = client (Proxy :: Proxy CreateUser)
        loginUser = client (Proxy :: Proxy LoginUser)
        createNote = client (Proxy :: Proxy CreateNote)
        getNotes = client (Proxy :: Proxy GetNotes)
        getNotesByName = client (Proxy :: Proxy GetNotesByName)
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
          it "should create another valid user" $ \_port -> do
            result <- runClientM (createUser userWPSample2) clientEnv
            case result of
              Left (FailureResponse _ response) ->
                liftIO $ print $ responseBody response
              Right res -> liftIO $ print res
            result `shouldBe` Right 2

    describe "POST /user" $
      it "should fail to create user and report that email exists" $ \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[ExistingEmail,ExistingUserName]"
          Right res -> liftIO $ print res

    describe "POST /login" $
      it "should fail to authenticate unactivated user" $ \_port -> do
        result <- runClientM (loginUser $ UserLogin "nini" "password") clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "Incorrect username or password, or account not yet activated"
          Right res -> liftIO $ print res

    -- Use makeAuthToken directly here for testing.
    -- In the app this is called when the user successfully authenticates,
    -- and an 'authToken' is returned to them for creating and reading notes.
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
          it "should successfully create a new note for second user" $ \_port -> do
            now <- getCurrentTime
            let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) now
            case decodeUtf8' token of
              Left _ -> liftIO $ print "couldn't decode token"
              Right token' -> do
                let note = NoteInput  "laurie" "some note" "do something good"
                result <- runClientM (createNote note (Just $ Token token')) clientEnv
                case result of
                  Left (FailureResponse _ response) ->
                    liftIO $ print $ responseBody response
                  Right res -> liftIO $ print res
                result `shouldBe` Right (toSqlKey 2)
    
    describe "POST /note" $
     it "should fail to create new note with user not found error" $ \_port -> do
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
               responseBody response `shouldBe` "User not found" 
             Right res -> liftIO $ print res

    describe "POST /note" $
         it "it should not create note and fail with user not authorised" $ \_port -> do
           now <- getCurrentTime
           let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) now
           case decodeUtf8' token of
             Left _ -> liftIO $ print "couldn't decode token"
             Right token' -> do
               let note = NoteInput  "laurie" "some note" "do something else"
               result <- runClientM (createNote note (Just $ Token token')) clientEnv
               case result of
                 Left (FailureResponse _ response) -> do
                   liftIO $ print $ responseBody response
                   responseBody response `shouldBe` "Not Authorised - use your own user name to create new notes" 
                 Right res -> liftIO $ print res

    describe "POST /note" $
     it "should not create note, should return token expired error" $ \_port -> do
       let time = makeUTCTime (2021, 10, 10) (14, 15, 0)
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           let note = NoteInput  "nini" "another note" "do something good"
           result <- runClientM (createNote note (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) -> do
               liftIO $ print $ responseBody response
               let response' = LB.take 29 $ responseBody response
               response' `shouldBe` "Token not valid: TokenExpired"
             Right res ->
               liftIO $ print res

    describe "GET /notes" $
     it "should retrieve all notes" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotes Nothing Nothing (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right res -> do
               let numberOfNotes = length res
               numberOfNotes `shouldBe` 2

    describe "GET /notes?start=2021-10-6" $
     it "should retrieve all notes created after given date" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotes (Just "2021-10-6") Nothing (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right res -> do
               let numberOfNotes = length res
               numberOfNotes `shouldBe` 2

    describe "GET /notes?start=2021-10-6&end=2021-10-7" $
     it "should retrieve all notes created between two dates" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotes (Just "2021-10-6") (Just "2021-10-7") (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right res -> do
               let numberOfNotes = length res
               numberOfNotes `shouldBe` 0

    describe "GET /notes?end=2021-10-7" $
     it "should retrieve all notes created before given date" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample2}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotes Nothing (Just "2021-10-7") (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right res -> do
               let numberOfNotes = length res
               numberOfNotes `shouldBe` 0
    
    describe "GET /notes/laurie" $
     it "should successfully retrieve note created by supplied user name" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotesByName "laurie" Nothing Nothing (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right [Entity (NoteKey (SqlBackendKey resultKey)) _] -> do
               liftIO $ print resultKey
               resultKey `shouldBe` 2

    describe "GET /notes/laurie?start=2021-8-2" $
     it "should successfully retrieve notes created by supplied user name after given date" $ \_port -> do
       time <- getCurrentTime
       let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) time
       case decodeUtf8' token of
         Left _ -> liftIO $ print "couldn't decode token"
         Right token' -> do
           result <- runClientM (getNotesByName "laurie" (Just "2021-8-2") Nothing (Just $ Token token')) clientEnv
           case result of
             Left (FailureResponse _ response) ->
               liftIO $ print $ responseBody response
             Right res -> do
               let numberOfNotes = length res
               numberOfNotes `shouldBe` 1

    describe "GET /notes/laurie?end=2021-8-2" $
      it "should successfully retrieve notes created by supplied user name before given date" $ \_port -> do
        time <- getCurrentTime
        let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = nameSample}) time
        case decodeUtf8' token of
          Left _ -> liftIO $ print "couldn't decode token"
          Right token' -> do
            result <- runClientM (getNotesByName "laurie" Nothing (Just "2021-8-2") (Just $ Token token')) clientEnv
            case result of
              Left (FailureResponse _ response) ->
                liftIO $ print $ responseBody response
              Right res -> do
                let numberOfNotes = length res
                numberOfNotes `shouldBe` 0

withUserApp :: (Port -> IO ()) -> IO ()
withUserApp = testWithApplication noteApp

noteApp :: IO Application
noteApp = serve noteApi <$> noteServer'

noteServer' :: IO (Server NoteAPI)
noteServer' = initialEnv >>= \env -> pure $ hoistAppServer env 

userWPSample1 :: UserWithPassword
userWPSample1 = UserWithPassword "nini" 100 "nini@mail.com" "password"

userWPSample2 :: UserWithPassword
userWPSample2 = UserWithPassword "laurie" 50 "laurie@mail.com" "password"

malformedUser :: UserWithPassword
malformedUser = UserWithPassword "lo" 130 "lou@@mail.com" "pass"

createdEntityUserFromSample1 :: Entity User
createdEntityUserFromSample1 = Entity (toSqlKey 1) createdUserFromSample1

createdUserFromSample1 :: User
createdUserFromSample1 = User nameSample ageSample emailSample (Just False)

makeUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
makeUTCTime (year, mon, day) (hour, minute, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour minute sec))
