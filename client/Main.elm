import Http
import Task exposing (Task, andThen)
import TaskTutorial exposing (print, getCurrentTime)

import Hledger exposing (update, view, emptyModel, fetchEntries)
import StartApp.Simple exposing (start)

port runner : Task Http.Error ()
port runner = fetchEntries 
              `andThen` print 
              `andThen` \() -> getCurrentTime
              `andThen` print

main =
  start
    { model = emptyModel
    , update = update
    , view = view
    }
