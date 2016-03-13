module Hledger where

import Html exposing (..)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onClick, on, targetValue)

import Http 
import Task exposing (Task, andThen)

import Debug
import TaskTutorial exposing (..)

-- Auxiliary functions and variables
serviceUrl = "http://services.vicarie.in"

fetchEntries = Http.send (serviceUrl ++ "/entries")

-- MODEL

type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { description : String
                    , comment : String
                    , postings : (Posting, Posting)
                    }

type alias Model = { currentFields : JEntry
                   , restEntries: List JEntry
                   }

emptyJEntry = { description = ""
              , comment = ""
              , postings = ( { account =  "", amount = ""}
                           , { account = "", amount = ""})
              }

emptyModel : Model
emptyModel = { currentFields = emptyJEntry
             , restEntries = []
             }

-- UPDATE

type Action = AddNew | DeleteLast | FetchAll | ClearAll 
            | SetDesc String
            | SetComment String
            | SetAcc1 String
            | SetAcc2 String
            | SetAmnt1 String
            | SetAmnt2 String

update : Action -> Model -> Model
update action model =
  let fields = model.currentFields
  in
    case action of
      AddNew -> let newEntry = model.currentFields
                in  { model 
                      | restEntries = [newEntry] ++ model.restEntries
                      , currentFields = emptyJEntry
                    }
      DeleteLast -> let two = Debug.log "Delete last" 2
                    in model
      ClearAll -> emptyModel
      FetchAll -> model
      (SetDesc desc) -> let newFields = { fields | description = desc }
                        in { model | currentFields = newFields }
      (SetComment com)  -> let newFields = { fields | comment = com }
                           in { model | currentFields = newFields }
      (SetAcc1 acc) -> let (p1, p2) = fields.postings
                           newPostings = ({ p1 | account = acc }, p2)
                           newFields = { fields | postings = newPostings }
                       in { model | currentFields = newFields }
      (SetAcc2 acc) -> let (p1, p2) = fields.postings
                           newPostings = (p1, { p2 | account = acc })
                           newFields = { fields | postings = newPostings }
                       in { model | currentFields = newFields }
      (SetAmnt1 a1) -> let (p1, p2) = fields.postings
                           a1_ = "₹ " ++ a1
                           newPostings = ({ p1 | amount = a1_ }, p2)
                           newFields = { fields | postings = newPostings }
                       in { model | currentFields = newFields }
      (SetAmnt2 a2) -> let (p1, p2) = fields.postings
                           a2_ = "₹ " ++ a2
                           newPostings = (p1, { p2 | amount = a2_ })
                           newFields = { fields | postings = newPostings }
                       in { model | currentFields = newFields }
      
                       
-- VIEW
encodeModel : Model -> String
encodeModel model = 
  case .restEntries model of
    [] -> ""
    (entry::entries) -> encodeJEntry entry
                        ++ "\n\n" 
                        ++ encodeModel {model | restEntries = entries}
encodeJEntry : JEntry -> String
encodeJEntry entry = case (.postings entry) of
                      (p1, p2) -> .description entry ++ "\n" ++
                                  (if .comment entry == ""
                                   then "\n"
                                   else "\t; " ++ (.comment entry) ++ "\n") ++
                                  "  " ++ .account p1 ++ "   " ++ .amount p1 ++ "\n" ++
                                  "  " ++ .account p2 ++ "   " ++ .amount p2 ++ "\n"

view : Signal.Address Action -> Model -> Html
view address model =
  div [appStyle]
  [ div []
      [ div []
          [ input [ placeholder "Description"
                  , inputStyle
                  , on "input" targetValue (Signal.message address << SetDesc)
                  ]
              []
          ]
      , div []
          [ input [ placeholder "Comment"
                  , on "input" targetValue (Signal.message address << SetComment)
                  , inputStyle
                  ]
              []
          ]
      , div []
          [ input [ placeholder "Account Name"
                  , miniInputStyle
                  , on "input" targetValue (Signal.message address << SetAcc1) ]
              []
          , input [ placeholder "Amount (₹)"
                  , miniInputStyle
                  , on "input" targetValue (Signal.message address << SetAmnt1)]
              []
          ]
      , div []
          [ input [ placeholder "Account Name"
                  , miniInputStyle
                  , on "input" targetValue (Signal.message address << SetAcc2) ]
              []
          , input [ placeholder "Amount (₹)"
                  , miniInputStyle
                  , on "input" targetValue (Signal.message address << SetAmnt2) ] 
              []
          ]              
      ]
  , div [appStyle]
      [ button [ buttonStyle, onClick address AddNew ] [ text "Add" ]
      , button [ buttonStyle, onClick address DeleteLast ] [ text "Delete" ]
      , button [ buttonStyle, onClick address FetchAll ] [ text "Fetch" ]
      , button [ buttonStyle, onClick address ClearAll ] [ text "Clear" ]
      ]
  , div [statusBoxStyle] [ text (encodeJEntry model.currentFields ++ "\n" ++ encodeModel model) ]
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

