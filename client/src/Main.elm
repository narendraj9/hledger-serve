module Main ( main ) where

import HLedger exposing (init, update, view)
import HEffects exposing (modalMailbox, toastMailbox)

import Http
import Task exposing (Task)
import StartApp exposing (start)
import Effects exposing (Effects, Never)

app =
  start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }
main = app.html
    
port tasks : Signal (Task Never ())
port tasks = app.tasks

port modalRequests : Signal String
port modalRequests = modalMailbox.signal                     

port toastRequests : Signal String
port toastRequests = toastMailbox.signal
