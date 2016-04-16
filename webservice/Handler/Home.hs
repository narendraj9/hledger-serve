module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    addScript $ StaticR js_elm_js
    
    setTitle "Penguin's HLedger Client"
    $(widgetFile "homepage")

