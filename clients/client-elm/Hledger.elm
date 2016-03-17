
module Hledger where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)

import Http 
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Json.Encode as JsonEn exposing (string, list, Value)
import Task exposing (Task, andThen)
import TaskTutorial exposing (..)

-- Service info
serviceUri : String
serviceUri = "http://services.vicarie.in/"

-- Auxiliary functions [For fetching gifs]
getRandomGif : String -> Effects Action
getRandomGif topic = Http.get decodeUrl (randomUrl topic)
                   |> Task.toMaybe
                   |> Task.map NewGif
                   |> Effects.task

decodeUrl : Json.Decoder String
decodeUrl = Json.at ["data", "image_url"] Json.string

randomUrl : String -> String
randomUrl topic = Http.url "http://api.giphy.com/v1/gifs/random"
                  [ "api_key" => "dc6zaTOxFJmzC"
                  , "tag" => topic
                  ]

-- Auxiliary functions for talking to the webservice
decodePosting : Json.Decoder Posting
decodePosting = Json.object2 Posting ("account" := Json.string)
                                     ("amount" := Json.string)

decodeJEntry : Json.Decoder JEntry
decodeJEntry =  Json.object4 JEntry ("date" := Json.string)
                                    ("description" := Json.string)
                                    ("comment" := Json.string)
                                    ("postings" := Json.list decodePosting)

decodeJEntryList : Json.Decoder (List JEntry)
decodeJEntryList = Json.list decodeJEntry
                  
encodePosting : Posting -> Value
encodePosting posting = JsonEn.object [ ("account", string posting.account)
                                      , ("amount" , string posting.amount)
                                      ]

encodeJEntry : JEntry -> Value
encodeJEntry jentry = JsonEn.object [ ("date", string jentry.date)
                                    , ("description", string jentry.description)
                                    , ("comment", string jentry.comment)
                                    , ("postings", JsonEn.list
                                         (List.map encodePosting jentry.postings))
                                    ]

fetchAll : Effects Action
fetchAll = Http.get decodeJEntryList (serviceUri ++ "/entries")
         |> Task.toMaybe
         |> Task.map FetchedAll
         |> Effects.task

addNew : JEntry -> Effects Action
addNew jentry = Http.send Http.defaultSettings
                { verb = "POST"
                , url = serviceUri ++ "/entry"
                , headers = [ ("content-type", "application/json") ]         
                , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                }
              |> Http.fromJson decodeJEntryList
              |> Task.toMaybe
              |> Task.map AddedNew
              |> Effects.task
            
clearAll : Effects Action
clearAll = Http.send Http.defaultSettings
           { verb = "DELETE"
           , url = (serviceUri ++ "/entries")
           , headers = []
           , body = Http.empty
           }
         |> Http.fromJson decodeJEntryList
         |> Task.toMaybe
         |> Task.map ClearedAll
         |> Effects.task 
           
deleteLast : Effects Action
deleteLast = Http.post decodeJEntryList (serviceUri ++ "/delete") Http.empty
            |> Task.toMaybe
            |> Task.map DeletedLast
            |> Effects.task
            
getAPenguin : Effects Action
getAPenguin = getRandomGif "cute penguin"

-- Model
type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { date : String
                    , description : String
                    , comment : String
                    , postings : List Posting
                    }
type alias Model = { currentFields : JEntry
                   , restEntries: List JEntry
                   , imgUrl : String
                   }
                 
initialPostings : List Posting
initialPostings = [ { account =  "", amount = ""}
                  , { account = "", amount = ""}
                  ]
initialJEntry : JEntry
initialJEntry = { date = ""
                , description = ""
                , comment = ""
                , postings = initialPostings
                }
initialModel : Model
initialModel = { currentFields = initialJEntry
               , restEntries = []
               , imgUrl = "_assets/penguin.png"
               }
-- Auxiliary functions that query the model
getPostings2 : JEntry -> (Posting, Posting, List Posting)
getPostings2 jentry = let postings = jentry.postings
                          defaultPosting = { account = ""
                                           , amount = ""
                                           }
                          p1 = Maybe.withDefault defaultPosting (List.head postings)
                          ptail = Maybe.withDefault [] (List.tail postings)
                          p2 = Maybe.withDefault defaultPosting (List.head ptail)
                          rest = Maybe.withDefault [] (List.tail ptail)
                      in (p1, p2, rest)

-- Init
init : (Model, Effects Action)
init = ( initialModel
       , getAPenguin
       )

-- UPDATE

type Action = AddNew
            | DeleteLast
            | FetchAll
            | ClearAll
            | SetDesc String
            | SetComment String
            | SetAccountA String
            | SetAccountB String
            | SetAmountA String
            | SetAmountB String
            | NewGif (Maybe String)
            | AddedNew (Maybe (List JEntry))
            | DeletedLast (Maybe (List JEntry))
            | FetchedAll (Maybe (List JEntry))
            | ClearedAll (Maybe (List JEntry))

update : Action -> Model -> (Model, Effects Action)
update action model =
  let fields = model.currentFields
      (p1, p2, rest) = getPostings2 fields
    
      -- funtions to avoid typing in the Action case branches         
      noEf model = (model, Effects.none)
      setEntries serverEntries =
        { model
          | restEntries = Maybe.withDefault model.restEntries serverEntries
        }
  in
    case action of
      -- Application --> Server
      AddNew -> let newEntry = model.currentFields
                in  ( model
                    , Effects.batch [ addNew newEntry
                                    , getAPenguin
                                    ]
                    )
      DeleteLast -> ( model, deleteLast )
      ClearAll -> ( model, clearAll )
      FetchAll -> ( model, fetchAll )

      -- Server --> Application
      AddedNew serverEntries -> let newModel = setEntries serverEntries 
                                in noEf { newModel | currentFields = initialJEntry }
      DeletedLast serverEntries -> noEf <| setEntries serverEntries
      FetchedAll serverEntries -> noEf <| setEntries serverEntries
      ClearedAll serverEntries -> noEf <| setEntries serverEntries

      -- Form fields --> Model
      (SetDesc desc) -> let newFields = { fields | description = desc }
                        in noEf { model | currentFields = newFields }
                              
      (SetComment com)  -> let newFields = { fields | comment = com }
                           in noEf { model | currentFields = newFields }
                              
      (SetAccountA acc) -> let newPostings = { p1 | account = acc } :: p2 :: rest
                               newFields = { fields | postings = newPostings }
                           in noEf { model | currentFields = newFields }
                              
      (SetAccountB acc) -> let newPostings = p1 :: { p2 | account = acc } :: rest
                               newFields = { fields | postings = newPostings }
                           in noEf { model | currentFields = newFields }
                              
      (SetAmountA a1) -> let a1_ = if a1 /= "" then "₹ " ++ a1 else a1
                             newPostings = { p1 | amount = a1_ } :: p2 :: rest
                             newFields = { fields | postings = newPostings }
                       in noEf { model | currentFields = newFields }
                              
      (SetAmountB a2) -> let a2_ = if a2 /= "" then "₹ " ++ a2 else a2
                             newPostings = p1 :: { p2 | amount = a2_ } :: rest
                             newFields = { fields | postings = newPostings }
                         in noEf { model | currentFields = newFields }
    -- A new penguin gif just arrived
      (NewGif maybeUrl) -> noEf { model
                                  | imgUrl = Maybe.withDefault model.imgUrl maybeUrl }
                       
-- VIEW
viewModel : Model -> Html
viewModel model = 
  case .restEntries model of
    [] -> div [] []
    (entry::entries) -> div []
                            [ i [class "materical-icons"] []
                            , viewJEntry entry
                            , viewModel {model | restEntries = entries}
                            ]
                        
viewJEntry : JEntry -> Html
viewJEntry entry = let (p1, p2, rest) = getPostings2 entry
                       description = entry.description
                       comment = entry.comment
                     in
                       div []
                           [ div []
                               [ text description ]
                           , div []
                               [ text (String.concat [ "          ;"
                                                     , comment
                                                     ]) ]
                           , div []
                               [ text (String.concat [ "   "
                                                     , .account p1
                                                     , "   "
                                                     , .amount p1
                                                     ])
                               ]
                                      
                           ]
  
view : Signal.Address Action -> Model -> Html
view address model =
  let (p1, p2, rest) = getPostings2 model.currentFields
  in
  div [class "container"]
  [ div [ class "divider" ]
      []
  , div [ class "row" ]
      [ div [ class "row" ]
          [ div [ class "col" ]
              [ a [ href "#" ] 
                  [ img [ class "responsive-img"
                        , imgStyle
                        , src model.imgUrl
                        ] 
                      [] 
                  ]
              ]
          , div [ class "col" ]
                [ text "Penguin's \n Hledger Client" ]
          ]
      ]
  , div [ class "row" ]
      [ div []
          [ input [ placeholder "Description"
                  , inputStyle
                  , value model.currentFields.description
                  , on "input" targetValue (Signal.message address << SetDesc)
                  ]
              []
          ]
      , div []
          [ textarea [ placeholder "Comment (Optional)"
                     , rows 10
                     , value model.currentFields.comment
                     , on "input" targetValue (Signal.message address << SetComment)
                     , inputStyle
                     ]
              []
          ]
      , div []
          [ input [ placeholder "Account Name"
                  , miniInputStyle
                  , value p1.account
                  , on "input" targetValue (Signal.message address << SetAccountA) ]
              []
          , input [ placeholder "Amount (₹)"
                  , miniInputStyle
                  , value p1.amount
                  , on "input" targetValue (Signal.message address << SetAmountA)]
              []
          ]
      , div []
          [ input [ placeholder "Account Name"
                  , miniInputStyle
                  , value p2.account
                  , on "input" targetValue (Signal.message address << SetAccountB) ]
              []
          , input [ placeholder "Amount (₹)"
                  , miniInputStyle
                  , value p2.amount
                  , on "input" targetValue (Signal.message address << SetAmountB) ] 
              []
          ]              
      ]
  , div [class "row"]
      [ a [ class "waves-effect waves-light btn"
          , onClick address AddNew 
          ] 
          [ i [ class "material-icons left"  ] 
              [ text "cloud" ]
          , text "Add"
          ]
      , a [ class "waves-effect waves-light btn"
          , onClick address DeleteLast 
          ] 
          [ i [ class "material-icons left"  ] 
              [ text "cloud" ]
          , text "Delete"
          ]
      , a [ class "waves-effect waves-light btn"
          , onClick address FetchAll 
          ]
          [ i [ class "material-icons left"  ] 
                        [ text "cloud" ]
          , text "Fetch"
          ]
      , a [ class "waves-effect waves-light btn"
          , onClick address ClearAll 
          ] 
          [ i [ class "material-icons left"  ] 
              [ text "cloud" ]
          , text "Clear"
          ]
      ]
  , div [statusBoxStyle] [ div []
                             [ div [] [ viewJEntry model.currentFields ]
                             , div [] [ viewModel model ]
                             ]
                         ]
  ]

-- Styling
(=>) = (,)

appStyle : Attribute
appStyle = 
  style
    [ "class" => "container"
    , "font-size" => "20px"
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
    , "border" => "4px"
    , "height" => "40px"
    , "font-size" => "20px"
    ]
statusBoxStyle : Attribute
statusBoxStyle =
  style 
    [ "white-space" => "pre"
    ]


bannerStyle : Attribute
bannerStyle =
  style
    [ "font-size" => "22px"
    , "background" => "#ee6e73"
    ]

imgStyle : Attribute
imgStyle =
  style
    [ "width" => "auto"
    , "border-top" => "4px"
    , "height" => "64px"
    ]