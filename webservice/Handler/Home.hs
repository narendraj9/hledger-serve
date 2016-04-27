module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  defaultLayout $ do
    setTitle "Penguin's HLedger Client"
    -- | This is because $(widgetFile "homepage") isn't doing what it should
    toWidget [julius|
function elmBootstrap () {
    var app = Elm.fullscreen(Elm.Main);
    // Subscribe to request for showing modals
    app.ports.modalRequests.subscribe(function (modalId) {
        $(modalId).openModal({
            dismissible: true,
            opacity: 0.5, 
            in_duration: 300, 
            out_duration: 200,
        });
    });

    // Susbscribe to requests for showing toasts
    app.ports.toastRequests.subscribe(function (text) {
        Materialize.toast(text, 10000 ,"rounded");
    });
}

elmBootstrap();

    |]


