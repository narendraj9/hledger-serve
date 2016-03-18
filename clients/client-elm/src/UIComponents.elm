module UIComponents ( viewForm
                    , viewJEntryList
                    , viewButtons
                    , (=>)
                    , htmlNav
                    , htmlPreloader
                    ) where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)

import Model exposing (..)

-- Function for setting display attribute
displayStyle : String -> Attribute
displayStyle value = style [ ("display", value) ]

-- Common Elm code for icons
icon classNames iconName = i [ class classNames ] 
                           [ text iconName ]
-- Instead of typing all the html manually, call these functions
materialIcon = icon "material-icons"  
prefixIcon = icon "material-icons prefix" 


viewEmptyState : Signal.Address Action -> Model -> Html
viewEmptyState address model =  div [ class "container"
                                    , displayStyle model.ui.entryListDisp
                                    ]
                                [ div [ class "row" ]
                                    [ div [ class "col s12"]
                                        [ div [ class "card-panel grey lighten-5 z-depth-1" ]
                                            [ div [ class "row" ]
                                                [ div [ class "col offset-s2 s8 center" ]
                                                    [ a [ class "btn btn-large"
                                                        , noTouchToSearchStyle
                                                        , onClick address ShowForm 
                                                        ] 
                                                        [ icon "material-icons large" "add_alert" ]
                                                    ]
                                                ]
                                            , div [ class "row" ]
                                                [ div [ class "col offset-s2 s8 center  orange-text flow-text" ] 
                                                    [ text "Time to add a journal entry! :)" ]
                                                ]
                                            , div [ class "row" ]
                                                [ div [ class "col offset-s2 s8 center" ]
                                                    [ img [ class "responsive-img"
                                                          , src "_assets/empty-state-bear.png"
                                                          ]
                                                        []
                                                    ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]



-- Auxiliary functions for building the view
viewJEntryList : Signal.Address Action -> Model -> Html
viewJEntryList address model = 
  case .restEntries model of
    [] -> viewEmptyState address model
    entries -> div [ class "container"
                   , displayStyle model.ui.entryListDisp ] 
               (List.map htmlJEntry entries)
                        
htmlJEntry : JEntry -> Html
htmlJEntry entry =   let (p1, p2, rest) = getPostings2 entry
                         date = entry.date
                         description = entry.description
                         comment = String.trim entry.comment
                         commentDisplay = if not (String.isEmpty comment)
                                          then style [ ("display", "block") ]
                                          else style [ ("display", "none") ]
                         htmlPosting p = div [ class "col offset-s1 s12" ]
                                         [ span [ class "black-text" ]
                                                  [ text (p.account ++
                                                            (if (String.isEmpty (String.trim p.amount))
                                                             then ""
                                                             else "   ₹ " ++ p.amount))
                                                  ]
                                         ]
                     in
                       div [ class "row" ]
                             [ div [ class "col s12 m8 offset-m2 z-depth-1"
                                   , entryStyle ]
                                     [ div [ class "col s12"]
                                               [ span [] [ text (date ++ " ") ]
                                               , span [] [ text description ]
                                               ]
                                     , div [ class "col s8 offset-s2"
                                           , commentDisplay ]
                                               [ blockquote [ class "right s8"
                                                            , blockquoteStyle
                                                            ]
                                                   [ p [] [text comment ] ]
                                               ]

                                     , htmlPosting p1
                                     , htmlPosting p2
                                     ]
                             ]

viewButtons : Signal.Address Action -> Html
viewButtons address = let fabStyle = style [ ("bottom" , "45px")
                                           , ("right" , "24px")
                                           ]
              in div [ class "fixed-action-btn horizontal"
                     , fabStyle
                     , noTouchToSearchStyle ]
                   [ a [ class "btn-floating btn-large red" ]
                       [ i [ class "large material-icons"
                           , noTouchToSearchStyle ] 
                           [ text "mode_edit" ]
                       ]
                   , ul []
                       [ li []
                           [ a [ class "btn-floating btn-small red darken-2"
                               , noTouchToSearchStyle
                               , onClick address ClearAll 
                               ] 
                               [ materialIcon "delete_sweep" ]
                           ]
                       , li []
                           [ a [ class "btn-floating btn-small red"
                               , noTouchToSearchStyle
                               , onClick address DeleteLast 
                               ] 
                               [ materialIcon "remove" ]
                           ]
                       , li []  
                           [ a [ class "btn-floating btn-small blue"
                               , noTouchToSearchStyle
                               , onClick address FetchAll 
                               ]
                               [ materialIcon "restore" ]
                           ]
                       , li [] 
                           [ a [ class "btn-floating btn-small teal"
                               , noTouchToSearchStyle
                               , onClick address ShowForm 
                               ] 
                               [ materialIcon "add" ]
                           ]
                       ]
                   ]

htmlNav : Model -> Html
htmlNav model = 
  div [ class "row indigo lighten-4" ]
        [ div [ class "col s6" ]
            [ a [ href "#" ] 
                [ img [ class "responsive-img z-depth-3"
                      , imgStyle
                      , src model.ui.imgUrl
                      ] 
                    [] 
                ]
            ]
        , div [ class "col small-text right-text right z-depth-3" ]
            [ div [class "flow-text black-text"] [ text "Penguin's" ]
            , div [class "flow-text"] [ text "Hledger Client" ]
            ]
        ]

htmlPreloader : Model -> Html
htmlPreloader model = div [ class "progress"
                          , displayStyle model.ui.preloaderDisp ]
                        [ div [ class "indeterminate" ]
                            []
                        ]
              
viewForm : Signal.Address Action -> Model -> Html
viewForm address model =  
  let  fields = model.currentFields 
       (p1, p2, rest) = getPostings2 fields
       onInput tag = on "input" targetValue (Signal.message address << tag)
       description = div [ class "row" ]
                     [ div [ class "input-field col s12" ]
                         [ input [ id "description-field"
                                 , class "validate"
                                 , type' "text"
                                 , value model.currentFields.description
                                 , onInput SetDesc
                                 ]
                             []
                         , label [ for "description-field" ]
                             [ text "Title" ]
                         , prefixIcon "description"
                         ]
                     ]
       comments = div [ class "row" ]
                  [ div [ class "input-field col s12" ]
                      [ textarea [ id "comment-field"
                                 , class "materialize-textarea"
                                 , value model.currentFields.comment
                                 , onInput SetComment
                                 ]
                          []
                      , label [ for "comment-field" ]
                          [ text "Comments (Optional)" ]
                      , prefixIcon "comment"
                      ]
                  ]
       account l i tag = div [ class "input-field col s6" ]
                         [ input [ id i
                                 , class "validate"
                                 , type' "text"
                                 , onInput tag
                                    ]
                                []
                            , label [ for i ]
                                [ text l ]
                            ] 
       amount l i tag = div [ class "input-field col s6" ]
                            [ input [ id i
                                    , class "validate"
                                    , type' "number"
                                    , onInput tag
                                    ]
                                []
                            , label [ for i ]
                                [ text l ]
                            ] 

  in
    div [ class "row"
        , displayStyle model.ui.formDisp
        ]
      [ Html.form [ class "col s12" ]
          [ description
          , comments
          , div [ class "row" ]
              [ account "Account #1" "acc-1" SetAccountA
              , amount  "Amount (Rs)" "amount-1" SetAmountA
              ]
          , div [ class "row" ]
              [ account "Account #2" "acc-2" SetAccountB
              , amount "Amount (Rs)" "amount-2" SetAmountB
              ]
          , div [ class "row right" ]
              [ a [ class "btn btn-small teal"
                  , onClick address AddNew 
                  , noTouchToSearchStyle
                  ] 
                  [ icon "material-icons right" "send"
                  , text "Submit"
                  ]
              ]
          ]
      ]
                               

-- Styling
(=>) = (,)

appStyle : Attribute
appStyle = 
  style []


cardStyle : Attribute
cardStyle = 
  style
    [ "padding" => "10px"
    ]

bannerStyle : Attribute
bannerStyle =
  style
    [ "background" => "#ee6e73"
    ]

imgStyle : Attribute
imgStyle =
  style
    [ "width" => "auto"
    , "padding-top" => "4px"
    , "height" => "78px"
    ]

-- An attempt to fix "touch to search" on buttons
noTouchToSearchStyle : Attribute
noTouchToSearchStyle = 
  style 
    [ ("role", "button")
    , ("tabindex", "1")
    , ("-webkit-user-select", "none")
    ]

-- Attributes exactly similar to a card-panel  
entryStyle : Attribute
entryStyle =
  style
    [ ("transition", "box-shadow .25s")
    , ("padding", "10px")
    , ("margin", "0.5rem 0 1rem 0")
    , ("border-radius", "2px")
    ]

-- materialize.css blockquote with thinner border    
blockquoteStyle : Attribute
blockquoteStyle =
  style
    [ ("border-left", "2px solid #ee6e73")
    ]

