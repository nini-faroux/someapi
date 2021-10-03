{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Api 
  ( UserAPI
  , CreateUser
  , GetUser
  , GetUsers
  , userApi
  , createUser
  , getUser
  , getUsers
  , loginUser
  , activateUserAccount
  , getNotes
  , createNote
  , getNotesByName
  ) where

import Servant
import Servant.Multipart (MultipartForm, MultipartData, Mem, inputs, iValue)
import RIO (Text, Int64, encodeUtf8, decodeUtf8', throwIO, liftIO)
import RIO.Time (getCurrentTime)
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
import UserTypes (makeName)
import NoteTypes (NoteRequest(..))
import qualified Query

type UserAPI =
       GetUsers
  :<|> GetUser
  :<|> CreateUser
  :<|> ActivateUser
  :<|> LoginUser
  :<|> GetNotes
  :<|> CreateNote
  :<|> GetNotesByName

type GetUser = "user" :> Capture "userid" (P.Key User) :> Get '[JSON] (Entity User)
type GetUsers = "users" :> Get '[JSON] [Entity User]
type CreateUser = "user" :> ReqBody '[JSON] UserWithPassword :> Post '[JSON] Int64
type ActivateUser = "activate" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] (Maybe (Entity User))
type LoginUser = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] Token
type GetNotes = "notes" :> Header "Authorization" Token :> Get '[JSON] [Entity Note]
type CreateNote = "note" :> ReqBody '[JSON] NoteInput :> Header "Authorization" Token :> Post '[JSON] (P.Key Note)
type GetNotesByName = "notes" :> Capture "username" Text :> Header "Authorization" Token :> Get '[JSON] [Entity Note]

userApi :: Proxy UserAPI
userApi = Proxy

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
  name <- validName loginName
  exists <- nameExists name
  if not exists then throwIO err401 { errBody = authErrorMessage }
  else do 
    auth <- Query.getAuth name
    case auth of
      [Entity _ (Auth _uid hashPass)] -> do
        let pass' = mkPassword loginPassword
        case checkPassword pass' hashPass of
          PasswordCheckFail -> throwIO err401 { errBody = authErrorMessage }
          PasswordCheckSuccess -> do
            now <- getCurrentTime
            let token = makeAuthToken (Scope {protectedAccess = True, tokenUserName = name}) now
            case decodeUtf8' token of
              Left _ -> return $ Token ""
              Right token' -> return $ Token token'
      _ -> throwIO err401 { errBody = authErrorMessage }
    where
      validName name =
        case makeName name of
          Failure _err -> throwIO err400 { errBody = "Invalid name" }
          Success name' -> return name'
      nameExists name = do
        mUser <- Query.getUserByName name
        case mUser of
          Nothing -> return False
          _user -> return True
      authErrorMessage = "Incorrect username or password, or account not yet activated"

getUsers :: App [Entity User]
getUsers = Query.getUsers

getUser :: P.Key User -> App (Entity User)
getUser userId = do
  mUser <- Query.getUserById userId
  case mUser of
    Nothing -> throwIO err404 { errBody = "User not found" }
    Just user -> return user

getNotes :: Maybe Token -> App [Entity Note]
getNotes = notesRequest Query.getNotes Nothing GetNoteRequest

getNotesByName :: Text -> Maybe Token -> App [Entity Note]
getNotesByName noteAuthor = notesRequest (query noteAuthor) (Just noteAuthor) GetNotesByNameRequest
  where
    query author =
      case makeName author of
        Failure err -> throwIO err400 { errBody = LB.fromString $ show err }
        Success validName -> do
          mUser <- Query.getUserByName validName
          case mUser of
            Nothing -> throwIO err404 { errBody = "User not found" }
            Just _user -> Query.getNotesByName validName

createNote :: NoteInput -> Maybe Token -> App (P.Key Note)
createNote note@NoteInput{..} = notesRequest (insertNote note) (Just noteAuthor) CreateNoteRequest
  where
    insertNote noteInput = parseNote noteInput >>= \validNote -> Query.insertNote validNote

notesRequest :: App a -> Maybe Text -> NoteRequest -> Maybe Token -> App a
notesRequest _ _ _ Nothing = throwIO err401 { errBody = "No token found" }
notesRequest query mName requestType (Just (Token token)) = do
  eScope <- liftIO . decodeAndValidateAuth $ encodeUtf8 $ T.init $ T.drop 8 token
  case eScope of
    Left err -> throwIO err400 { errBody = LB.fromString err }
    Right Scope {..} -> check requestType query mName protectedAccess tokenUserName
    where
      check request query' mName' protectedAccess tokenName
        | not protectedAccess = throwIO err403 { errBody = "Not Authorised" }
        | request == GetNoteRequest || request == GetNotesByNameRequest = query'
        | validName mName' tokenName = query'
        | otherwise = throwIO err403 { errBody = "Not Authorised - use your own user name to create new notes" }
      validName Nothing _tName = False
      validName (Just name') tName =
        case makeName name' of
          Failure _err -> False
          Success validName' -> validName' == tName

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
