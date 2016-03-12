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
type JournalAPI = GetEntries :<|> PostEntry
type GetEntries = "entries" :> Get '[JSON] [JEntry]
type PostEntry = "entry" :> ReqBody '[JSON] JEntry :> Post '[] ()

-- | GET /entries
handleGetEntries :: JStore -> Server GetEntries
handleGetEntries store = liftIO $ readTVarIO store

-- | POST /entry                            
handlePostEntry :: JStore -> Server PostEntry
handlePostEntry store jentry = liftIO $ atomically $ modifyTVar store (jentry:)

journalAPI :: Proxy JournalAPI
journalAPI = Proxy

handleJournalAPI :: JStore -> Server JournalAPI
handleJournalAPI store = handleGetEntries store :<|> handlePostEntry store

app :: JStore -> Application
app store = serve journalAPI $ handleJournalAPI store

main :: IO ()
main = do store <- newTVarIO entries
          run 8000 (app store)


-- Sample entries for initial testing
entries :: [JEntry]
entries =
  [ JEntry "Had breakfast at Well Cafe" "It was awesome. \n Just amazin!"
    [Posting "expenses:eating:breakfast" "230", Posting "assets:wallet:cash" "-230"]
  ]

