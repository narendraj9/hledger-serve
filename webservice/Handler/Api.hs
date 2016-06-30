module Handler.Api where

import Import
import Data.Aeson (encode)
import qualified Data.Text.Lazy.Encoding as DTE

getApiCopyR :: Handler Html
getApiCopyR = do
  currentUserId <- requireAuthId
  entries <- runDB $ selectList [ EntryUserId ==. currentUserId ] [Desc EntryNumber]
  let entriesString = DTE.decodeUtf8 (encode entries)
  defaultLayout $ do
    
    setTitle "Penguin's HLedger Client"
    toWidget [julius|
          new Clipboard('.btn');
      |]

    toWidget [hamlet|
      <textarea id="bar" rows="10"> #{entriesString}
 
      <button class="btn" data-clipboard-action="copy" data-clipboard-target="#bar">
          Cut to clipboard
    |]


