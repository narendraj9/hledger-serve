import Http
import Task exposing (Task, andThen)
import TaskTutorial exposing (print, getCurrentTime)

import Hledger exposing (update, view, emptyModel, fetchEntries)
import StartApp.Simple exposing (start)

fetchTask : Task Http.Error ()
fetchTask = fetchEntries 
              `andThen` print 
              `andThen` \() -> getCurrentTime
              `andThen` print
port runner : Task Http.Error ()
port runner = fetchTask

main =
  start
    { model = emptyModel
    , update = update
    , view = view
    }
