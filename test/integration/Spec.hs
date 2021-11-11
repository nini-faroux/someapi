{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Api
import App
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Fixed (Pico)
import Data.String (IsString)
import Data.Validation
import qualified Database.Persist as P
import Database.Persist.Sql (BackendKey (SqlBackendKey), Entity (..), toSqlKey)
import Network.HTTP.Client hiding (Proxy, responseBody)
import Network.Wai.Handler.Warp
import Parse.NoteTypes (DateInput (..))
import Parse.UserTypes (emailSample, nameSample, nameSample2)
import RIO
import RIO.Time (TimeOfDay (TimeOfDay), UTCTime (UTCTime), fromGregorian, getCurrentTime, timeOfDayToTime)
import Servant
import Servant.Client
import System.Environment (getEnv, setEnv, unsetEnv)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Web.JWT (AuthToken (..), Scope (..), Token (..))
import Web.Model
import Web.Server (hoistAppServer, noteServer)

{- | Sets the required env vars locally, runs the tests, then unsets the env vars.
  To run the tests:
  $ ./setup-integration-test.sh
  $ stack test :integration-test
-}
main :: IO ()
main = setEnvVarsForTests >> hspec apiInteractionTest >> unsetEnvVarsForTests

-- | Tests user interactions with the api
apiInteractionTest :: Spec
apiInteractionTest = do
  let createUser = client (Proxy :: Proxy CreateUser)
      loginUser = client (Proxy :: Proxy LoginUser)
      createNote = client (Proxy :: Proxy CreateNote)
      getNotes = client (Proxy :: Proxy GetNotes)
      getNotesByName = client (Proxy :: Proxy GetNotesByName)
  baseUrl <- runIO $ parseBaseUrl "localhost"
  manager <- runIO $ newManager defaultManagerSettings
  let clientEnv = mkClientEnv manager (baseUrl {baseUrlPort = 8080})
  describe "POST /user" $
    it "should not create invalid user, and should report all validation errors" $
      \_port -> do
        result <- runClientM (createUser malformedUser) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[InvalidName,InvalidEmail,InvalidPassword]"
          Right res -> liftIO $ print res
          _ -> liftIO $ print result
  describe "POST /user" $
    it "should create a valid user" $
      \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right 1
  describe "POST /user" $
    it "should create another valid user" $
      \_port -> do
        result <- runClientM (createUser userWPSample2) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right 2
  describe "POST /user" $
    it "should fail to create user and report that email exists" $
      \_port -> do
        result <- runClientM (createUser userWPSample1) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "[ExistingEmail,ExistingUserName]"
          Right res -> liftIO $ print res
  describe "POST /login" $
    it "should fail to authenticate unactivated user" $
      \_port -> do
        result <- runClientM (loginUser $ UserLogin "nini" "password") clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "Incorrect username or password, or account not yet activated"
          Right res -> print "ok"
  -- Use makeAuthToken directly here for testing.
  -- In the app this is called when the user successfully authenticates,
  -- and an 'authToken' is returned to them for creating and reading notes.
  describe "POST /note" $
    it "should successfully create a new note" $
      \_port -> do
        token <- makeAuthToken nameSample
        let note = NoteInput "nini" "some note" "do something good"
        result <- runClientM (createNote note (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right (toSqlKey 1)
  describe "POST /note" $
    it "should successfully create a new note for second user" $
      \_port -> do
        token <- makeAuthToken nameSample2
        let note = NoteInput "laurie" "some note" "do something good"
        result <- runClientM (createNote note (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> liftIO $ print res
        result `shouldBe` Right (toSqlKey 2)
  describe "POST /note" $
    it "should fail to create new note with user not found error" $
      \_port -> do
        token <- makeAuthToken nameSample
        let note = NoteInput "major" "some note" "do something else"
        result <- runClientM (createNote note (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "User not found"
          Right res -> liftIO $ print res
  describe "POST /note" $
    it "it should not create note and fail with user not authorised" $
      \_port -> do
        token <- makeAuthToken nameSample
        let note = NoteInput "laurie" "some note" "do something else"
        result <- runClientM (createNote note (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) -> do
            liftIO $ print $ responseBody response
            responseBody response `shouldBe` "Not Authorised - use your own user name to create new notes"
          Right res -> liftIO $ print res
  describe "GET /notes" $
    it "should retrieve all notes" $
      \_port -> do
        token <- makeAuthToken nameSample2
        result <- runClientM (getNotes Nothing Nothing (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 2
  describe "GET /notes?start=2021-10-6" $
    it "should retrieve all notes created after given date" $
      \_port -> do
        token <- makeAuthToken nameSample2
        result <- runClientM (getNotes (Just "2021-10-6") Nothing (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 2
  describe "GET /notes?start=2021-10-6&end=2021-10-7" $
    it "should retrieve all notes created between two dates" $
      \_port -> do
        token <- makeAuthToken nameSample2
        result <- runClientM (getNotes (Just "2021-10-6") (Just "2021-10-7") (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 0
  describe "GET /notes?end=2021-10-7" $
    it "should retrieve all notes created before_ given date" $
      \_port -> do
        token <- makeAuthToken nameSample2
        result <- runClientM (getNotes Nothing (Just "2021-10-7") (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 0
  describe "GET /notes/laurie" $
    it "should successfully retrieve note created by supplied user name" $
      \_port -> do
        token <- makeAuthToken nameSample
        result <- runClientM (getNotesByName "laurie" Nothing Nothing (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right [Entity (NoteKey (SqlBackendKey resultKey)) _] -> do
            liftIO $ print resultKey
            resultKey `shouldBe` 2
  describe "GET /notes/laurie?start=2021-8-2" $
    it "should successfully retrieve notes created by supplied user name after given date" $
      \_port -> do
        token <- makeAuthToken nameSample
        result <- runClientM (getNotesByName "laurie" (Just "2021-8-2") Nothing (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 1
  describe "GET /notes/laurie?end=2021-8-2" $
    it "should successfully retrieve notes created by supplied user name before given date" $
      \_port -> do
        token <- makeAuthToken nameSample
        result <- runClientM (getNotesByName "laurie" Nothing (Just "2021-8-2") (Just token)) clientEnv
        case result of
          Left (FailureResponse _ response) ->
            liftIO $ print $ responseBody response
          Right res -> do
            let numberOfNotes = length res
            numberOfNotes `shouldBe` 0

{- | Set the env vars for the duration of the tests
 based on the .env file contents.
 Unset them when the tests are complete
-}
setEnvVarsForTests :: IO ()
setEnvVarsForTests = do
  s <- readFile ".env"
  let keyValues = getNamesAndValues s
  setEnvs keyValues

unsetEnvVarsForTests :: IO ()
unsetEnvVarsForTests = do
  s <- readFile ".env"
  let keyValues = getNamesAndValues s
  unsetEnvs keyValues

setEnvs :: [(String, String)] -> IO ()
setEnvs = mapM_ $ uncurry setEnv

unsetEnvs :: [(String, String)] -> IO ()
unsetEnvs = mapM_ (\(k, _) -> unsetEnv k)

getNamesAndValues :: String -> [(String, String)]
getNamesAndValues xs = go xs []
  where
    go [] acc = acc
    go xs acc = go (dropNameAndValue xs) (getNameAndValue xs : acc)

getNameAndValue :: String -> (String, String)
getNameAndValue s =
  let name = getVarName s
      s' = dropName s
      value = getVarValue s'
   in (name, value)

getVarName :: String -> String
getVarName = takeWhile (/= '=')

getVarValue :: String -> String
getVarValue = takeWhile (/= '\n')

dropNameAndValue :: String -> String
dropNameAndValue = dropValue . dropName

dropName :: String -> String
dropName = drop 1 . dropWhile (/= '=')

dropValue :: String -> String
dropValue = dropWhile (== '\n') . dropWhile (/= '\n')

-- | Sample data
userWPSample1 :: UserWithPassword
userWPSample1 = UserWithPassword "nini" "nini@mail.com" "password"

userWPSample2 :: UserWithPassword
userWPSample2 = UserWithPassword "laurie" "laurie@mail.com" "password"

malformedUser :: UserWithPassword
malformedUser = UserWithPassword "lo" "lou@@mail.com" "pass"

createdEntityUserFromSample1 :: Entity User
createdEntityUserFromSample1 = Entity (toSqlKey 1) createdUserFromSample1

createdUserFromSample1 :: User
createdUserFromSample1 = User nameSample emailSample (Just False)
