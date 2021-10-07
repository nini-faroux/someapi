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
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Esqueleto.Experimental
 (Entity(..), fromSqlKey)
import Data.Password.Bcrypt (PasswordCheck(..), mkPassword, checkPassword)
import qualified Data.ByteString.Lazy.UTF8 as LB
import Data.Validation (Validation(..))
import App (App)
import Model
   (User(..), UserWithPassword(..), UserLogin(..), Auth(..), Note(..), NoteInput(..),
   Scope(..), Token(..), makePassword)
import Email (sendActivationLink)
import JWT (makeAuthToken, decodeAndValidateAuth, decodeAndValidateUser)
import UserValidation (parseUser)
import NoteValidation (parseNote)
import UserTypes (Name, makeName)
import NoteTypes (NoteRequest(..), DayInput(..), makeValidDayText, makeValidDay, makeValidName)
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

createUser :: UserWithPassword -> App Int64
createUser uwp@UserWithPassword {..} = do
    user <- parseUser uwp
    pass <- liftIO $ makePassword password
    newUserId <- Query.insertUser user
    _ <- Query.insertAuth newUserId pass
    liftIO $ sendActivationLink user
    return $ fromSqlKey newUserId

loginUser :: UserLogin -> App Token
loginUser UserLogin {..} = do
  name <- validName' loginName
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
    validName' name =
      case makeName name of
        Failure _err -> throwIO err400 { errBody = "Invalid name" }
        Success name' -> return name'
    getExistingName name = do
      mUser <- Query.getUserByName name
      case mUser of
        Nothing -> throwIO err401 { errBody = authErrorMessage }
        Just _user -> return name
    authErrorMessage = "Incorrect username or password, or account not yet activated"

getNotesByName :: Text -> Maybe Text -> Maybe Text -> Maybe Token -> App [Entity Note]
getNotesByName noteAuthor mStart mEnd = notesRequest (query noteAuthor mStart mEnd) (Just noteAuthor) GetNotesByNameRequest
  where
    query author Nothing Nothing = getNotesBetweenDates (Just author) Nothing Nothing
    query author js@(Just _startDate) Nothing = getNotesBetweenDates (Just author) js Nothing
    query author Nothing je@(Just _endDate) = getNotesBetweenDates (Just author) Nothing je
    query author js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates (Just author) js je

getNotes :: Maybe Text -> Maybe Text -> Maybe Token -> App [Entity Note]
getNotes mStartDate mEndDate = notesRequest (query mStartDate mEndDate) Nothing GetNoteRequest
  where
    query Nothing Nothing = Query.getNotes
    query js@(Just _startDate) Nothing = getNotesBetweenDates Nothing js Nothing
    query Nothing je@(Just _endDate) = getNotesBetweenDates Nothing Nothing je
    query js@(Just _startDate) je@(Just _endDate) = getNotesBetweenDates Nothing js je

getNotesBetweenDates :: Maybe Text -> Maybe Text -> Maybe Text -> App [Entity Note]
getNotesBetweenDates Nothing Nothing Nothing = Query.getNotes
getNotesBetweenDates justAuthor@(Just _author) Nothing Nothing = do
  mname <- makeValidName justAuthor
  Query.getNotesByName mname
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
      mname <- makeValidName mName
      makeQuery mname start end

getStartEndAndName :: Maybe Text -> Text -> Text -> (Text -> App Text) -> App (Maybe Name, Text, Text)
getStartEndAndName mName start end makeDay =
  makeValidDay start >>= \s -> makeDay end >>= \e -> makeValidName mName >>= \mn -> return (mn, s, e)

makeQuery :: Maybe Name -> Text -> Text -> App [Entity Note]
makeQuery mName start end
  | start > end = throwIO err400 { errBody = "Error: end date is before start date" }
  | otherwise = queryBetweenDates mName start end

queryBetweenDates :: Maybe Name -> Text -> Text -> App [Entity Note]
queryBetweenDates Nothing start end = Query.getNotesBetweenDates start end
queryBetweenDates (Just name) start end = Query.getNotesBetweenDatesWithName name start end

createNote :: NoteInput -> Maybe Token -> App (P.Key Note)
createNote note@NoteInput{..} = notesRequest (insertNote note) (Just noteAuthor) CreateNoteRequest
  where
    insertNote noteInput = parseNote noteInput >>= \validNote -> Query.insertNote validNote

notesRequest :: App a -> Maybe Text -> NoteRequest -> Maybe Token -> App a
notesRequest _ _ _ Nothing = throwIO err401 { errBody = "No token found" }
notesRequest query mName requestType (Just (Token token)) = do
  eScope <- decodeToken token
  case eScope of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right Scope {..} -> check requestType query mName protectedAccess tokenUserName
    where
      check request query' mName' protectedAccess tokenName
        | not protectedAccess = throwIO err403 { errBody = "Not Authorised" }
        | request == GetNoteRequest || request == GetNotesByNameRequest || request == GetNotesByDayRequest = query'
        | validName' mName' tokenName = query'
        | otherwise = throwIO err403 { errBody = "Not Authorised - use your own user name to create new notes" }
      validName' Nothing _tName = False
      validName' (Just name') tName =
        case makeName name' of
          Failure _err -> False
          Success vName -> vName == tName
      -- Need to trim the token to account for the 'Bearer' prefix 
      -- otherwise in that case it will raise a decoding error
      decodeToken token'
        | hasBearerPrefix token' = liftIO . decodeAndValidateAuth $ encodeUtf8 $ T.init $ T.drop 8 token'
        | otherwise = liftIO . decodeAndValidateAuth $ encodeUtf8 token'
      hasBearerPrefix token' = T.take 6 token' == "Bearer"

activateUserAccount :: MultipartData Mem -> App (Maybe (Entity User))
activateUserAccount formData = do
  let token = getFormInput formData
  eUser <- liftIO . decodeAndValidateUser $ encodeUtf8 token
  case eUser of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right user -> do
      Query.updateUserActivatedValue user.userEmail user.userName
      Query.getUserByEmail user.userEmail

getFormInput :: MultipartData Mem -> Text
getFormInput formData = token
  where
    nameValuePairs = inputs formData
    token = maybe "" iValue $ headMaybe nameValuePairs
