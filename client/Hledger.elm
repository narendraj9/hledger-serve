module Hledger where

import Html exposing (..)
import Html.Attributes exposing (style, placeholder)
import Html.Events exposing (onClick)

import Debug

-- MODEL

type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { description : String
                    , comment : String
                    , postings : List Posting
                    }
type alias Model = List JEntry

emptyJEntry = { description = ""
              , comment = ""
              , postings = []
              }
emptyModel : List JEntry
emptyModel = []

-- UPDATE

type Action = AddNew | DeleteLast | FetchAll | ClearAll 

update : Action -> Model -> Model
update action model =
  case action of
    AddNew -> let one = Debug.log "Add one new" 1
              in  emptyJEntry::model
    DeleteLast -> let two = Debug.log "Delete last" 2
                  in model
    ClearAll -> let three = Debug.log "Clear" 3
                in model
    FetchAll -> let four = Debug.log "Fetch" 4
                in model

-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div [appStyle]
  [ div [inputBoxStyle]
      [ div []
          [ input [ placeholder "Description"
                  , inputBoxStyle
                  ]
              []
          ]
      , div []
          [ input [ placeholder "Comment"
                  , inputBoxStyle
                  ]
              []
          ]
      , div [postingBoxStyle]
          [ input [ placeholder "Account Name", postingInputStyle ] []
          , input [ placeholder "Amount (₹)", postingInputStyle ] []
          ]
      , div [postingBoxStyle]
          [ input [ placeholder "Account Name", postingInputStyle ] []
          , input [ placeholder "Amount (₹)" , postingInputStyle] []
          ]              
      ]
  , div [buttonListStyle]
      [ button [ buttonStyle, onClick address AddNew ] [ text "Add" ]
      , button [ buttonStyle, onClick address DeleteLast ] [ text "Delete" ]
      , button [ buttonStyle, onClick address FetchAll ] [ text "Fetch" ]
      , button [ buttonStyle, onClick address ClearAll ] [ text "Clear" ]
      ]
  , div [] [ text (toString model) ]
  ]

-- Styling
(=>) = (,)

appStyle : Attribute
appStyle =
  style
    [ "font-size" => "20px"
    , "font-family" => "arial"
    ]
    
inputBoxStyle : Attribute
inputBoxStyle =
  style
    [ "width" => "100%"
    ]

postingBoxStyle : Attribute
postingBoxStyle =
  style
    [ "width" => "100%"
    ]

postingInputStyle : Attribute
postingInputStyle =
  style
    [ "width" => "40%"
    ]

buttonStyle : Attribute
buttonStyle =
  style
    [ "font-size" => "15px"
    , "font-family" => "arial"
    , "width" => "25%"
    , "height" => "30px"
    ]

buttonListStyle : Attribute
buttonListStyle =
  style
    [ "width" => "100%"
    ]
    


