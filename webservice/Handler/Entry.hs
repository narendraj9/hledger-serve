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
  entries <- case maybeCurrentUser of
                 Just currentUser -> do 
                   _ <- runDB $ insertEntity $ entry { entryNumber = (userCount currentUser) }
                   -- ^ Insert entry received in request body with next entry number
                   runDB $ update currentUserId [ UserCount +=. 1 ]
                   -- ^ Increment the max entry number for the current user
                   runDB $ selectList [ EntryUserId ==. currentUserId ] []
                 Nothing -> return []
  returnJson entries

-- | Update an already existing entry 
putEntryR :: Handler Value
putEntryR = do
  (currentUserId, _, entry) <- getRequestByUser
  maybeEntryEntity <- runDB $ getBy $ UniqueEntry currentUserId (entryNumber entry)
  case maybeEntryEntity of
    Nothing -> sendResponseStatus status404 ("ENTRY NOT FOUND" :: Text)
    Just (Entity eid _) -> runDB $ replace eid entry
  entries <- runDB $ selectList [ EntryUserId ==. currentUserId ] []
  returnJson entries

-- | Delete an entry form the database 
deleteEntryR :: Handler Value
deleteEntryR = do
  (currentUserId, _, entry) <- getRequestByUser
  _ <- runDB $ deleteBy $ UniqueEntry currentUserId (entryNumber entry)
  entries <- runDB $ selectList [ EntryUserId ==. currentUserId ] []
  returnJson entries
  
