
import Hledger exposing (update, view, emptyModel)
import StartApp.Simple exposing (start)


main =
  start
    { model = emptyModel
    , update = update
    , view = view
    }
