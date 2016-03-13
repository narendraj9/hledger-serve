module Hledger where

import Html exposing (..)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onClick)

import Http 
import Task exposing (Task, andThen)

import Debug
import TaskTutorial exposing (..)

-- Auxiliary functions and variables
serviceUrl = "http://localhost:80/"

fetchEntries : Task Http.Error String
fetchEntries = Http.getString (serviceUrl ++ "/entries")

-- MODEL

type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { description : String
                    , comment : String
                    , postings : List Posting
                    }

type alias Model = { currentFields : JEntry
                   , restEntries: List JEntry
                   }

emptyJEntry = { description = "We rented two bicycles"
              , comment = " No comments. "
              , postings = [ { account =  "food", amount = "232"}
                           , { account = "wallet", amount = "-232"}]
              }

emptyModel : Model
emptyModel = { currentFields = emptyJEntry
             , restEntries = []
             }

-- UPDATE

type Action = AddNew | DeleteLast | FetchAll | ClearAll 

update : Action -> Model -> Model
update action model =
  case action of
    AddNew -> let one = Debug.log "Add one new" 1
              in  {model | restEntries = [emptyJEntry] ++ model.restEntries }
    DeleteLast -> let two = Debug.log "Delete last" 2
                  in model
    ClearAll -> let three = Debug.log "Clear" 3
                in emptyModel
    FetchAll -> let four = Debug.log "" 4
                in model

-- VIEW
encodeModel : Model -> String
encodeModel model = 
  case .restEntries model of
    [] -> ""
    (entry::entries) -> encodeEntry entry
                        ++ "\n\n" ++ encodeModel {model | restEntries = entries}
encodeEntry : JEntry -> String
encodeEntry entry = case (.postings entry) of
                      [p1, p2] -> .description entry ++ "\n" ++
                                  "\t; " ++ .comment entry ++ "\n" ++
                                  "  " ++ .account p1 ++ "   " ++ .amount p1 ++ "\n" ++
                                  "  " ++ .account p2 ++ "   " ++ .amount p2 ++ "\n"
                      [] -> "\n"
                      (h::t) -> "\n"

view : Signal.Address Action -> Model -> Html
view address model =
  div [appStyle]
  [ div []
      [ div []
          [ input [ placeholder "Description"
                  , inputStyle
                  ]
              []
          ]
      , div []
          [ input [ placeholder "Comment"
                  , inputStyle
                  ]
              []
          ]
      , div []
          [ input [ placeholder "Account Name", miniInputStyle ] []
          , input [ placeholder "Amount (₹)", miniInputStyle ] []
          ]
      , div []
          [ input [ placeholder "Account Name", miniInputStyle ] []
          , input [ placeholder "Amount (₹)", miniInputStyle ] []
          ]              
      ]
  , div [appStyle]
      [ button [ buttonStyle, onClick address AddNew ] [ text "Add" ]
      , button [ buttonStyle, onClick address DeleteLast ] [ text "Delete" ]
      , button [ buttonStyle, onClick address FetchAll ] [ text "Fetch" ]
      , button [ buttonStyle, onClick address ClearAll ] [ text "Clear" ]
      ]
  , div [statusBoxStyle] [ text (encodeModel model) ]
  ]

-- Styling
(=>) = (,)

appStyle : Attribute
appStyle = 
  style 
    [ "font-size" => "20px"
    , "font-family" => "arial"
    ]
inputStyle : Attribute
inputStyle = 
  style
    [ "width" => "100%"
    , "height" => "40px"
    , "font-size" => "20px"
    , "padding" => "5px"
    ]
miniInputStyle : Attribute
miniInputStyle = 
  style
    [ "width" => "45%"
    , "height" => "40px"
    , "font-size" => "20px"
    , "padding" => "5px"
    ]
     
buttonStyle : Attribute
buttonStyle = 
  style 
    [ "width" => "25%"
    , "height" => "40px"
    , "font-size" => "20px"
    ]
statusBoxStyle : Attribute
statusBoxStyle =
  style 
    [ "white-space" => "pre"
    ]

