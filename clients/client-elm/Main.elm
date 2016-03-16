import Http
import Task exposing (Task)
import TaskTutorial exposing (print, getCurrentTime)
import StartApp exposing (start)
import Effects exposing (Effects, Never)

import Hledger exposing (init, update, view)

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
