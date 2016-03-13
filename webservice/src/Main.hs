{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

import Control.Monad.Trans
import Control.Concurrent.STM

data Posting = Posting
               { account :: String
               , amount :: String
               } deriving (Show, Eq, Generic)
instance ToJSON Posting
instance FromJSON Posting

data JEntry = JEntry
              { description :: String
              , comment :: String
              , postings :: [Posting]
              } deriving (Eq, Show, Generic)
instance ToJSON JEntry
instance FromJSON JEntry

type JStore = TVar [JEntry]
type JournalAPI = ClearEntries :<|> GetEntries :<|> PostEntry :<|> DeleteEntry
type ClearEntries = "entries" :> Delete '[] ()
type GetEntries = "entries" :> Get '[JSON] [JEntry]
type PostEntry = "entry" :> ReqBody '[JSON] JEntry :> Post '[JSON] [JEntry]
type DeleteEntry = "delete" :> Post '[JSON] [JEntry]

-- | DELETE /entries
handleClearEntries :: JStore -> Server ClearEntries
handleClearEntries store = liftIO $ atomically $ modifyTVar store (const [])

-- | GET /entries
handleGetEntries :: JStore -> Server GetEntries
handleGetEntries store = liftIO $ readTVarIO store

-- | POST /entry                            
handlePostEntry :: JStore -> Server PostEntry
handlePostEntry store jentry = liftIO $ atomically $ do modifyTVar store (jentry:)
                                                        readTVar store

-- | POST /delete
handleDeleteEntry :: JStore -> Server DeleteEntry
handleDeleteEntry store = let safeTail [] = []
                              safeTail (x:xs) = xs
                          in liftIO $ atomically $ do modifyTVar store safeTail
                                                      readTVar store
journalAPI :: Proxy JournalAPI
journalAPI = Proxy

handleJournalAPI :: JStore -> Server JournalAPI
handleJournalAPI store = handleClearEntries store
  :<|> handleGetEntries store
  :<|> handlePostEntry store
  :<|> handleDeleteEntry store

app :: JStore -> Application
app store = serve journalAPI $ handleJournalAPI store

main :: IO ()
main = do store <- newTVarIO entries
          run 8000 (app store)


-- Sample entries for initial testing
entries :: [JEntry]
entries =
  [ JEntry { description = "We rented bicylces."
           , comment =  "It wasn't as easy as it sounds."
           , postings = [ Posting { account = "expenses:travel:commute"
                                 , amount = "600"
                                 }
                       , Posting { account =  "assets:wallet:cash"
                                 , amount = "-600"
                                 }
                       ]
           }
  ]

