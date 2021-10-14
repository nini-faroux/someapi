{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api 
  ( NoteAPI
  , CreateUser
  , CreateNote
  , GetNotes
  , GetNotesByName
  , LoginUser
  , noteApi
  , createUser
  , loginUser
  , activateUserAccount
  , getNotes
  , createNote
  , getNotesByName
  ) where

import RIO
import Servant
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO.Time (getCurrentTime, toGregorian, utctDay)
import RIO.List (headMaybe)
import Database.Esqueleto.Experimental (Entity(..), Key, fromSqlKey)
import App (App)
import Model (User(..), UserWithPassword(..), UserLogin(..), Note(..), NoteInput(..))
import Email (sendActivationLink)
import JWT (Scope(..), Token(..), verifyAuthToken, verifyUserToken)
import UserValidation (parseUser)
import NoteValidation (parseNote)
import UserTypes (Name)
import NoteTypes (NoteRequest(..), DayInput(..), makeValidDayText, makeValidDay, makeValidName)
import Authenticate (makeAuthToken', getAuth, makePassword, checkPassword', checkUserCredentials, checkNameExists)
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
  :> Post '[JSON] (Key Note)
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
  logInfo $ "Attempting to authenticate user: " <> displayShow loginName
  name <- makeValidName loginName
  existingName <- checkNameExists name
  hashPass <- getAuth existingName
  _passCheck <- checkPassword' loginPassword hashPass
  makeAuthToken' existingName

-- | Endpoint for creating new notes, requires an active auth token
-- The 'noteAuthor' field in the request's body must be the same as your own user name (the one you are logged in with)
-- Otherwise the request will be rejected
-- Example request:
-- curl -X POST 'localhost:8000/note' \
-- -H 'Authorization: Bearer "<your token>"' \
-- -H 'Content-Type: application/json' \
-- -d '{ "noteAuthor" : "<your userName>", "noteTitle" : "some title", "noteBody": "do something good"}'
createNote :: NoteInput -> Maybe Token -> App (Key Note)
createNote note@NoteInput{..} mToken = do
  (existingName, scope) <- checkUserCredentials mToken noteAuthor
  notesRequest (insertNote note) (Just existingName) CreateNoteRequest scope
  where
    insertNote = Query.insertNote <=< parseNote

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
  notesRequest (query existingName mStart mEnd) (Just existingName) GetNoteRequest scope
  where
    query author Nothing Nothing = getNotesBetweenDates (Just author) Nothing Nothing
    query author js@(Just _startDate) Nothing = getNotesBetweenDates (Just author) js Nothing
    query author Nothing je@(Just _endDate) = getNotesBetweenDates (Just author) Nothing je
    query author js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates (Just author) js je

-- | Endpoint for when the user clicks on the email activation link
-- The user is activated allowing them to authenticate
activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  user <- verifyUserToken $ getFormInput formData
  Query.updateUserActivatedValue user.userEmail user.userName
  Query.getUserByEmail user.userEmail
  where
    getFormInput :: MultipartData Mem -> Text
    getFormInput formData' = token
      where
        nameValuePairs = inputs formData'
        token = maybe "" iValue $ headMaybe nameValuePairs

notesRequest :: App a -> Maybe Name -> NoteRequest -> Scope -> App a
notesRequest query mName requestType Scope {..}
  | not protectedAccess = throwIO err403 { errBody = "Not Authorised" }
  | requestType == GetNoteRequest = query
  | matchingName mName tokenUserName = query
  | otherwise = throwIO err403 { errBody = errorMessage }
  where
    matchingName Nothing _tokenName = False
    matchingName (Just userName) tokenUserName' = userName == tokenUserName'
    errorMessage = "Not Authorised - use your own user name to create new notes"

getNotesBetweenDates :: Maybe Name -> Maybe Text -> Maybe Text -> App [Entity Note]
getNotesBetweenDates Nothing Nothing Nothing = Query.getNotes
getNotesBetweenDates (Just author) Nothing Nothing = Query.getNotesByName author
getNotesBetweenDates mName (Just startDate) (Just endDate) = do
  (start, end) <- getStartAndEndParams startDate endDate makeValidDay
  makeQuery mName start end
getNotesBetweenDates mName (Just startDate) Nothing = do
  time <- liftIO getCurrentTime
  let (year, month, day) = toGregorian $ utctDay time
  end <- makeValidDayText (DayInput year month day)
  (start, end') <- getStartAndEndParams startDate end (const $ pure end)
  makeQuery mName start end'
getNotesBetweenDates mName Nothing (Just endDate) = do
  mStart <- Query.getFirstDay
  case mStart of
    Nothing -> Query.getNotes
    Just start -> do
      end <- makeValidDay endDate
      makeQuery mName start end

getStartAndEndParams :: Text -> Text -> (Text -> App Text) -> App (Text, Text)
getStartAndEndParams start end makeDay =
  makeValidDay start >>= \s -> makeDay end >>= \e -> return (s, e)

makeQuery :: Maybe Name -> Text -> Text -> App [Entity Note]
makeQuery mName start end
  | start > end = throwIO err400 { errBody = "Error: end date is before start date" }
  | otherwise = queryBetweenDates mName start end
  where
    queryBetweenDates :: Maybe Name -> Text -> Text -> App [Entity Note]
    queryBetweenDates Nothing start' end' = Query.getNotesBetweenDates start' end'
    queryBetweenDates (Just name) start' end' = Query.getNotesBetweenDatesWithName name start' end'
