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
import Servant hiding (throwError)
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO.List (headMaybe)
import Database.Esqueleto.Experimental (Entity(..), Key, fromSqlKey)
import Web.Model
  (User(..), UserWithPassword(..), UserLogin(..), Note(..), NoteInput(..), Database)
import Web.Email (Sendmail, sendActivationLink)
import Web.JWT (Scope(..), Token(..), VerifyAuthToken(..), MakeAuthToken(..), VerifyUserToken(..))
import Parse.UserValidation (parseUser)
import Parse.NoteValidation (parseNote)
import Parse.UserTypes (Name)
import Parse.NoteTypes
  (NoteRequest(..), MakeValidName(..), MakeValidDate(..), makeDateInput)
import Parse.Authenticate (MakePassword(..), makeAuthToken', getAuth, checkPassword', checkUserCredentials, CheckNameExists(..))
import Parse.Validation (ThrowError(..))
import qualified Web.Query as Query
import App (GetTime(..))

type NoteAPI =
       CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetNotes
  :<|> CreateNote
  :<|> GetNotesByName

-- | The API Endpoints
type CreateUser =
     "user"
  :> ReqBody '[JSON] UserWithPassword
  :> PostCreated '[JSON] Int64
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
  :> PostCreated '[JSON] (Key Note)
type GetNotesByName =
     "notes"
  :> Capture "username" Text
  :> QueryParam "start" Text
  :> QueryParam "end" Text
  :> Header "Authorization" Token
  :> Get '[JSON] [Entity Note]

noteApi :: Proxy NoteAPI
noteApi = Proxy

-- | Endpoint handler for creating a new user
-- * If the user input is valid
-- then the user will be created and an activation link sent to the given email
-- Example request:
-- curl -X POST 'localhost:8000/user' \
-- -H 'Content-Type: application/json' \
-- -d '{ "name": "<username>", "email": "<emailAddress>", "password": "password"}'
createUser :: ( Database env m
              , MakePassword m
              , Sendmail env m
              , ThrowError m
              ) => UserWithPassword -> m Int64
createUser uwp@UserWithPassword {..} = do
  user <- parseUser uwp
  pass <- makePassword password
  newUserId <- Query.insertUser user
  _ <- Query.insertAuth newUserId pass
  sendActivationLink user
  return $ fromSqlKey newUserId

-- | Endpoint handler for user authentication
-- * If the user is activated and supplies valid credentials
-- then the user will be sent an authentication JWT token
-- * The user will then be able to use this token to
-- create new notes and to read previous notes
-- Example request:
-- curl --location --request POST 'localhost:8000/login' \
-- -H 'Content-Type: application/json' \
-- -d '{ "loginName": "<userName>", "loginPassword": "password"}'
loginUser :: ( CheckNameExists m
             , Database env m
             , GetTime m
             , MakeAuthToken m
             , MakeValidName m
             , ThrowError m
             ) => UserLogin -> m Token
loginUser UserLogin {..} = do
  name <- makeValidName loginName
  existingName <- checkNameExists name
  hashPass <- getAuth existingName
  _passCheck <- checkPassword' loginPassword hashPass
  makeAuthToken' existingName

-- | Endpoint handler for creating new notes, requires an active auth token
-- The 'noteAuthor' field in the request's body must be the same as your own user name (the one you are logged in with)
-- Otherwise the request will be rejected
-- Example request:
-- curl -X POST 'localhost:8000/note' \
-- -H 'Authorization: Bearer "<your token>"' \
-- -H 'Content-Type: application/json' \
-- -d '{ "noteAuthor" : "<your userName>", "noteTitle" : "some title", "noteBody": "do something good"}'
createNote :: ( CheckNameExists m
              , Database env m
              , GetTime m
              , MakeValidName m
              , ThrowError m
              , VerifyAuthToken m
              ) => NoteInput -> Maybe Token -> m (Key Note)
createNote note@NoteInput{..} mToken = do
  (existingName, scope) <- checkUserCredentials mToken noteAuthor
  notesRequest (insertNote note) (Just existingName) CreateNoteRequest scope
  where
    insertNote = Query.insertNote <=< parseNote

-- | Endpoint handler for fetching notes, requires an active auth token for access
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
getNotes :: ( Database env m
            , GetTime m
            , MakeValidDate m
            , ThrowError m
            , VerifyAuthToken m
            ) => Maybe Text -> Maybe Text -> Maybe Token -> m [Entity Note]
getNotes mStartDate mEndDate mToken = do
  scope <- verifyAuthToken mToken
  notesRequest (query mStartDate mEndDate) Nothing GetNoteRequest scope
  where
    query Nothing Nothing = Query.getNotes
    query js@(Just _startDate) Nothing = getNotesBetweenDates Nothing js Nothing
    query Nothing je@(Just _endDate) = getNotesBetweenDates Nothing Nothing je
    query js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates Nothing js je

-- | Endpoint handler for fetching notes created by a specific author
-- * If no query parameters are specified
-- then all the notes from that author will be returned
-- Example: /notes/<authorName>
-- * If query parameters are provided
-- then the notes (for the specified user) will be returned using the same logic as the 'getNotes' endpoint
-- Examples:
-- /notes/<authorName>?start=2021-8-6&end=2021-10-6
-- /notes/<authorName>?start=2021-8-6
-- /notes/<authorName>?end=2021-10-6
getNotesByName :: ( CheckNameExists m
                  , Database env m
                  , GetTime m
                  , MakeValidDate m
                  , MakeValidName m
                  , ThrowError m
                  , VerifyAuthToken m
                  ) => Text -> Maybe Text -> Maybe Text -> Maybe Token -> m [Entity Note]
getNotesByName noteAuthor mStart mEnd mToken = do
  (existingName, scope) <- checkUserCredentials mToken noteAuthor
  notesRequest (query existingName mStart mEnd) (Just existingName) GetNoteRequest scope
  where
    query author Nothing Nothing = getNotesBetweenDates (Just author) Nothing Nothing
    query author js@(Just _startDate) Nothing = getNotesBetweenDates (Just author) js Nothing
    query author Nothing je@(Just _endDate) = getNotesBetweenDates (Just author) Nothing je
    query author js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates (Just author) js je

-- | Endpoint handler for when the user clicks on the email activation link
-- The user is activated allowing them to authenticate
activateUserAccount :: ( Database env m
                       , VerifyUserToken m
                       ) => MultipartData Mem -> m (Maybe (Entity User))
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

notesRequest :: (ThrowError m) => m a -> Maybe Name -> NoteRequest -> Scope -> m a
notesRequest query mName requestType Scope {..}
  | not protectedAccess = throwError err403 { errBody = "Not Authorised" }
  | requestType == GetNoteRequest = query
  | matchingName mName tokenUserName = query
  | otherwise = throwError err403 { errBody = errorMessage }
  where
    matchingName Nothing _tokenName = False
    matchingName (Just userName) tokenUserName' = userName == tokenUserName'
    errorMessage = "Not Authorised - use your own user name to create new notes"

getNotesBetweenDates :: ( Database env m
                        , GetTime m
                        , MakeValidDate m
                        , ThrowError m
                        ) => Maybe Name -> Maybe Text -> Maybe Text -> m [Entity Note]
getNotesBetweenDates Nothing Nothing Nothing = Query.getNotes
getNotesBetweenDates (Just author) Nothing Nothing = Query.getNotesByName author
getNotesBetweenDates mName (Just startDate) (Just endDate) = do
  (start, end) <- getStartAndEndParams startDate endDate
  makeQuery mName start end
getNotesBetweenDates mName (Just startDate) Nothing = do
  dayInput <- makeDateInput
  end <- makeValidDate (Right dayInput)
  (start, end') <- getStartAndEndParams startDate end
  makeQuery mName start end'
getNotesBetweenDates mName Nothing (Just endDate) = do
  mStart <- Query.getFirstDate
  case mStart of
    Nothing -> Query.getNotes
    Just start -> do
      end <- makeValidDate (Left endDate)
      makeQuery mName start end

getStartAndEndParams :: (MakeValidDate m) => Text -> Text -> m (Text, Text)
getStartAndEndParams start end =
  makeValidDate (Left start) >>= \s -> makeValidDate (Left end) >>= \e -> return (s, e)

makeQuery :: ( Database env m
             , ThrowError m
             ) => Maybe Name -> Text -> Text -> m [Entity Note]
makeQuery mName start end
  | start > end = throwError err400 { errBody = "Error: end date is before start date" }
  | otherwise = queryBetweenDates mName start end
  where
    queryBetweenDates :: (Database env m) => Maybe Name -> Text -> Text -> m [Entity Note]
    queryBetweenDates Nothing start' end' = Query.getNotesBetweenDates start' end'
    queryBetweenDates (Just name) start' end' = Query.getNotesBetweenDatesWithName name start' end'
