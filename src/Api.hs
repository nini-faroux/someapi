{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api 
  ( NoteAPI
  , CreateUser
  , CreateNote
  , noteApi
  , createUser
  , loginUser
  , activateUserAccount
  , getNotes
  , createNote
  , getNotesByName
  ) where

import Servant
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO (Text, Int64, encodeUtf8, decodeUtf8', throwIO, liftIO)
import RIO.Time (getCurrentTime, toGregorian, utctDay)
import RIO.List (headMaybe)
import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), fromSqlKey)
import Data.Password.Bcrypt (PasswordCheck(..), mkPassword, checkPassword)
import qualified Data.ByteString.Lazy.UTF8 as LB
import App (App)
import Model
   (User(..), UserWithPassword(..), UserLogin(..), Auth(..), Note(..), NoteInput(..),
   Scope(..), Token(..), makePassword)
import Email (sendActivationLink)
import JWT (makeAuthToken, decodeAndValidateUser, verifyAuthToken)
import UserValidation (parseUser)
import NoteValidation (parseNote)
import UserTypes (Name)
import NoteTypes (NoteRequest(..), DayInput(..), makeValidDayText, makeValidDay, makeValidNameM, makeValidName)
import qualified Query

type NoteAPI =
       CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetNotes
  :<|> CreateNote
  :<|> GetNotesByName

type CreateUser =
     "user"
  :> ReqBody '[JSON] UserWithPassword
  :> Post '[JSON] Int64
type ActivateUser =
     "activate"
  :> MultipartForm Mem (MultipartData Mem)
  :> Post '[JSON] (Maybe (Entity User))
type LoginUser =
     "login"
  :> ReqBody '[JSON] UserLogin
  :> Post '[JSON] Token
type GetNotes =
     "notes"
  :> QueryParam "start" Text
  :> QueryParam "end" Text
  :> Header "Authorization" Token
  :> Get '[JSON] [Entity Note]
type CreateNote =
     "note"
  :> ReqBody '[JSON] NoteInput
  :> Header "Authorization" Token
  :> Post '[JSON] (P.Key Note)
type GetNotesByName =
     "notes"
  :> Capture "username" Text
  :> QueryParam "start" Text
  :> QueryParam "end" Text
  :> Header "Authorization" Token
  :> Get '[JSON] [Entity Note]

noteApi :: Proxy NoteAPI
noteApi = Proxy

-- | Endpoint for creating a new user
-- * If the user input is valid
-- then the user will be created and an activation link sent to the given email
-- Example request:
-- curl -X POST 'localhost:8000/user' \
-- -H 'Content-Type: application/json' \
-- -d '{ "name": "<username>", "age": 90, "email": "<emailAddress>", "password": "password"}'
createUser :: UserWithPassword -> App Int64
createUser uwp@UserWithPassword {..} = do
    user <- parseUser uwp
    pass <- liftIO $ makePassword password
    newUserId <- Query.insertUser user
    _ <- Query.insertAuth newUserId pass
    liftIO $ sendActivationLink user
    return $ fromSqlKey newUserId

-- | Endpoint for user authentication
-- * If the user is activated and supplies valid credentials
-- then the user will be sent an authentication JWT token
-- * The user will then be able to use this token to
-- create new notes and to read previous notes
-- Example request:
-- curl --location --request POST 'localhost:8000/login' \
-- -H 'Content-Type: application/json' \
-- -d '{ "loginName": "<userName>", "loginPassword": "password"}'
loginUser :: UserLogin -> App Token
loginUser UserLogin {..} = do
  name <- makeValidName loginName
  existingName <- getExistingName name
  auth <- Query.getAuth existingName
  case auth of
    [Entity _ (Auth _uid hashPass)] -> do
      let pass' = mkPassword loginPassword
      case checkPassword pass' hashPass of
        PasswordCheckFail -> throwIO err401 { errBody = authErrorMessage }
        PasswordCheckSuccess -> do
          now <- getCurrentTime
          let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = existingName}) now
          case decodeUtf8' token of
            Left _ -> return $ Token ""
            Right token' -> return $ Token token'
    _ -> throwIO err401 { errBody = authErrorMessage }
   where
     authErrorMessage = "Incorrect username or password, or account not yet activated"

-- | Endpoint for creating new notes, requires an active auth token
-- The 'noteAuthor' field in the request's body must be the same as your own user name (the one you are logged in with)
-- Otherwise the request will be rejected
-- Example request:
-- curl -X POST 'localhost:8000/note' \
-- -H 'Authorization: Bearer "<your token>"' \
-- -H 'Content-Type: application/json' \
-- -d '{ "noteAuthor" : "<your userName>", "noteTitle" : "some title", "noteBody": "do something good"}'
createNote :: NoteInput -> Maybe Token -> App (P.Key Note)
createNote note@NoteInput{..} mToken = do
  (existingName, scope) <- checkUserCredentials mToken noteAuthor
  notesRequest (insertNote note) (Just existingName) CreateNoteRequest scope
  where
    insertNote noteInput = parseNote noteInput >>= \validNote -> Query.insertNote validNote

-- | Endpoint for fetching fetching notes, requires an active auth token for access
-- * If no query parameters are specified 
-- then it will return all the notes
-- Example request:
-- curl -X GET 'localhost:8000/notes' \
-- -H 'Authorization: Bearer "<your token>"'
-- * If both a (valid) start parameter and a (valid) end parameter are specified 
-- then all the notes within that time period will be returned
-- Example: /notes?start=2021-8-6&end=2021-10-6
-- /notes?start=2021-8-6&end=2021-10-7
-- * If only a (valid) start parameter is specified
-- then all the notes from that date on will be returned
-- Example: /notes?start=2021-8-6
-- * If only a (valid) end parameter is specified
-- then all the notes up until that date will be returned
-- Example: /notes?end=2021-10-6
getNotes :: Maybe Text -> Maybe Text -> Maybe Token -> App [Entity Note]
getNotes mStartDate mEndDate mToken = do
  scope <- verifyAuthToken mToken
  notesRequest (query mStartDate mEndDate) Nothing GetNoteRequest scope
  where
    query Nothing Nothing = Query.getNotes
    query js@(Just _startDate) Nothing = getNotesBetweenDates Nothing js Nothing
    query Nothing je@(Just _endDate) = getNotesBetweenDates Nothing Nothing je
    query js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates Nothing js je

-- | Endpoint for fetching notes created by a specific author
-- * If no query parameters are specified
-- then all the notes from that author will be returned
-- Example: /notes/<authorName>
-- * If query parameters are provided
-- then the notes (for the specified user) will be returned using the same logic as the 'getNotes' endpoint
-- Examples:
-- /notes/<authorName>?start=2021-8-6&end=2021-10-6
-- /notes/<authorName>?start=2021-8-6
-- /notes/<authorName>?end=2021-10-6
getNotesByName :: Text -> Maybe Text -> Maybe Text -> Maybe Token -> App [Entity Note]
getNotesByName noteAuthor mStart mEnd mToken = do
  (existingName, scope) <- checkUserCredentials mToken noteAuthor
  notesRequest (query noteAuthor mStart mEnd) (Just existingName) GetNotesByNameRequest scope
  where
    query author Nothing Nothing = getNotesBetweenDates (Just author) Nothing Nothing
    query author js@(Just _startDate) Nothing = getNotesBetweenDates (Just author) js Nothing
    query author Nothing je@(Just _endDate) = getNotesBetweenDates (Just author) Nothing je
    query author js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates (Just author) js je

-- | Endpoint for when the user clicks on the email activation link
-- The user is activated allowing them to authenticate
activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right user -> do
      Query.updateUserActivatedValue user.userEmail user.userName
      Query.getUserByEmail user.userEmail
  where
    getFormInput :: MultipartData Mem -> Text
    getFormInput formData' = token
      where
        nameValuePairs = inputs formData'
        token = maybe "" iValue $ headMaybe nameValuePairs

notesRequest :: App a -> Maybe Name -> NoteRequest -> Scope -> App a
notesRequest query mName requestType Scope {..} =
  check requestType query mName protectedAccess tokenUserName
  where
    check request query' mName' protectedAccess' tokenName
      | not protectedAccess' = throwIO err403 { errBody = "Not Authorised" }
      | request == GetNoteRequest || request == GetNotesByNameRequest || request == GetNotesByDayRequest = query'
      | matchingName mName' tokenName = query'
      | otherwise = throwIO err403 { errBody = "Not Authorised - use your own user name to create new notes" }
    matchingName Nothing _tName = False
    matchingName (Just name') tName = name' == tName

getNotesBetweenDates :: Maybe Text -> Maybe Text -> Maybe Text -> App [Entity Note]
getNotesBetweenDates Nothing Nothing Nothing = Query.getNotes
getNotesBetweenDates (Just author) Nothing Nothing = do
  name <- makeValidName author
  Query.getNotesByName name
getNotesBetweenDates mName (Just startDate) (Just endDate) = do
  (mname, start, end) <- getStartEndAndName mName startDate endDate makeValidDay
  makeQuery mname start end
getNotesBetweenDates mName (Just startDate) Nothing = do
  time <- liftIO getCurrentTime
  let (year, month, day) = toGregorian $ utctDay time
  end <- makeValidDayText (DayInput year month day)
  (mname, start, end') <- getStartEndAndName mName startDate end (const $ pure end)
  makeQuery mname start end'
getNotesBetweenDates mName Nothing (Just endDate) = do
  mStart <- Query.getFirstDay
  case mStart of
    Nothing -> Query.getNotes
    Just start -> do
      end <- makeValidDay endDate
      mname <- makeValidNameM mName
      makeQuery mname start end

getStartEndAndName :: Maybe Text -> Text -> Text -> (Text -> App Text) -> App (Maybe Name, Text, Text)
getStartEndAndName mName start end makeDay =
  makeValidDay start >>= \s -> makeDay end >>= \e -> makeValidNameM mName >>= \mn -> return (mn, s, e)

makeQuery :: Maybe Name -> Text -> Text -> App [Entity Note]
makeQuery mName start end
  | start > end = throwIO err400 { errBody = "Error: end date is before start date" }
  | otherwise = queryBetweenDates mName start end
  where
    queryBetweenDates :: Maybe Name -> Text -> Text -> App [Entity Note]
    queryBetweenDates Nothing start' end' = Query.getNotesBetweenDates start' end'
    queryBetweenDates (Just name) start' end' = Query.getNotesBetweenDatesWithName name start' end'

checkUserCredentials :: Maybe Token -> Text -> App (Name, Scope)
checkUserCredentials mToken author = do
  scope <- verifyAuthToken mToken
  name <- makeValidName author
  existingName <- getExistingName name
  return (existingName, scope)

getExistingName :: Name -> App Name
getExistingName name = do
  mUser <- Query.getUserByName name
  case mUser of
    Nothing -> throwIO err404 { errBody = "User not found" }
    Just _user -> return name
