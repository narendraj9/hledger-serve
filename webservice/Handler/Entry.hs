module Handler.Entry ( getEntryR
                     , postEntryR
                     , putEntryR
                     , deleteEntryR
                     ) where

import Import
import Data.Time.LocalTime

-- | Get today's date as a String [yyyy-mm-dd]
dateNow :: IO Text
dateNow = do now <- getZonedTime
             return $ pack $ formatTime defaultTimeLocale "%Y-%m-%d" now

-- | Get current user id, user object and entry in the json request body
getRequestByUser :: Handler (UserId, Maybe User, Entry)
getRequestByUser = do
    currentUserId <- requireAuthId
    -- ^ Get the persistent Id of the current user
    maybeCurrentUser <- runDB $ get currentUserId
    -- ^ This would never be nothing
    requestEntry <- (requireJsonBody :: Handler RequestEntry)
    let entry = Entry { entryNumber = requestEntryNumber requestEntry
                      , entryDate = requestEntryDate requestEntry
                      , entryDescription = requestEntryDescription requestEntry
                      , entryComment = requestEntryComment requestEntry
                      , entryPostings = requestEntryPostings requestEntry
                      , entryUserId = currentUserId
                      }
    return (currentUserId, maybeCurrentUser, entry)

getEntryR :: Handler Value
getEntryR = do
    currentUserId <- requireAuthId
    entries <- runDB $ selectList [ EntryUserId ==. currentUserId ] []
    returnJson entries
                 
postEntryR :: Handler Value
postEntryR = do
  (currentUserId, maybeCurrentUser, entry') <- getRequestByUser
  today <- liftIO dateNow
  let entry = entry' { entryDate = today }
  case maybeCurrentUser of
    Just currentUser -> do 
      _ <- runDB $ insertEntity $ entry { entryNumber = (userCount currentUser) }
      -- ^ Insert entry received in request body with next entry number
      runDB $ update currentUserId [ UserCount +=. 1 ]
      -- ^ Increment the max entry number for the current user
      sendResponseStatus status200 ("Entry Added" :: Text)
    Nothing -> sendResponseStatus status400 ("Email narendraj9@gmail.com" :: Text)
      -- ^ This will never happen. 

-- | Update an already existing entry 
putEntryR :: Handler ()
putEntryR = do
  (currentUserId, _, entry) <- getRequestByUser
  maybeEntryEntity <- runDB $ getBy $ UniqueEntry currentUserId (entryNumber entry)
  case maybeEntryEntity of
    Nothing ->
      sendResponseStatus status404 ("Entry Not Found" :: Text)
    Just (Entity eid _) -> do
      runDB $ replace eid entry 
      sendResponseStatus status200 ("Entry Updated" :: Text)

-- | Delete an entry form the database 
deleteEntryR :: Handler ()
deleteEntryR = do
  currentUserId <- requireAuthId
  entryNumberMaybe <- lookupGetParam "entry_number"
  case entryNumberMaybe of
    Just entryNumberS -> case (readMay entryNumberS) :: Maybe Int of
      (Just entryNumber) -> do 
        runDB $ deleteBy $ UniqueEntry currentUserId entryNumber
        sendResponseStatus status200 ("Entry Deleted" :: Text)
      Nothing ->
        sendResponseStatus status400 ("Required `entry_number` should be a number" :: Text)
    Nothing ->
      sendResponseStatus status400 ("Required query param `entry_number`" :: Text)
  
  
