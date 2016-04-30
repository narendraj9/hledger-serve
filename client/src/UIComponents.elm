module UIComponents ( viewForm
                    , viewJEntryList
                    , viewFloatingButtons
                    , (=>)
                    , htmlNav
                    , htmlFooter
                    , htmlPreloader
                    , htmlError
                    , viewPage
                    ) where


import Model exposing (..)

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)

-- Function for setting display attribute | Transition doesn't work. Why?
displayStyle : String -> Attribute
displayStyle value = let opacity = if value == "none"
                                      then "0"
                                      else "1"
                         transition = "opacity 0.0s ease 0.0s"
                     in style [ ("display", value)
                              , ("opacity", opacity)
                              -- Figure out why it isn't working
                              , ("transition", transition)
                              , ("-webkit-transition", transition)
                              , ("-moz-transition", transition)
                              ]

-- Common Elm code for icons
icon classNames iconName = i [ class classNames ] 
                           [ text iconName ]
-- Instead of typing all the html manually, call these functions
materialIcon = icon "tiny material-icons waves-effect waves-light"  
prefixIcon = icon "tiny material-icons waves-effect waves-light prefix" 


viewEmptyState : Signal.Address Action -> Model -> Html
viewEmptyState address model =  div [ class "container"
                                    , id "empty-state-bear"
                                    , displayStyle model.ui.entryListDisp
                                    ]
                                [ div [ class "row" ]
                                    [ div [ class "col s12"]
                                        [ div [ class "card-panel grey lighten-5 z-depth-1" ]
                                            [ div [ class "row" ]
                                                [ div [ class "col offset-s2 s8 center" ]
                                                    [ a [ class "waves-effect waves-light btn btn-large"
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
                                                          , src "static/images/empty-state-bear.png"
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
                   , id "entryList"
                   , displayStyle model.ui.entryListDisp 
                   ] 
               (List.map (\entry -> 
                            viewJEntry address entry)
                  entries)


-- | A `width` sized horizontal spacer
spacer : String -> Html
spacer width = span [ style [ ("display", "inline-block")
                            , ("width", width)
                            ]
                    ]
               [ text " " ]


-- | Html for a single journal entry
viewJEntry : Signal.Address Action -> JEntry -> Html
viewJEntry address entry =   
  let (p1, p2, rest) = getPostings2 entry
      date = entry.date
      description = entry.description
      comment = String.trim entry.comment
      commentDisplay = if not (String.isEmpty comment)
                       then style [ ("display", "block") ]
                       else style [ ("display", "none") ]
      htmlPosting p = div [ class "col offset-s1 s11" ]
                        [ span [ class "black-text" ]
                            [ text p.account ]
                        , span [ class "teal-text"
                               , whitespacePreWrap
                               ]
                            [ text (if (String.isEmpty (String.trim p.amount))
                                    then ""
                                    else "   ₹ " ++ p.amount)
                            ]
                        ]
  in
    div [ class "row entryItem offset-m2 z-depth-1 hoverable" 
        , entryStyle
        ]
      [ div [ class "col s12 blue-text"
            , style [ ("padding-left", "7em")
                    , ("text-indent", "-6em")
                    ]
            ] 
          [ text date 
          , spacer "1em"
          , span [ class "deep-purple-text accent-1" ]
              [ text description ]
          ]
      , div [ class "col s10 offset-s2 indigo-text lighten-5"
            , commentDisplay ]
          [ blockquote [ blockquoteStyle ]
              [ p [] [text comment ] ]
          ]
      , htmlPosting p1
      , htmlPosting p2
      , div [ class "divider" ] []
      , div [ class "col s6"]
          [ button [ class "btn-flat teal-text waves-effect waves-light modal-trigger left"
                   , noTouchToSearchStyle
                   , attribute "data-target" "confirm-modal" 
                   , onClick address (SetEntryToRemove entry)
                   ] 
              [ materialIcon "delete" ]
          ]
      , div [ class "col s6"]
          [ button [ class "btn-flat teal-text waves-effect waves-light right"
              , noTouchToSearchStyle
              , onClick address (EditEntry entry)
              ] 
              [ materialIcon "edit" ]
          ]
      ]
        
viewFloatingButtons : Signal.Address Action -> Html
viewFloatingButtons address = 
  let fabStyle = style [ ("bottom" , "45px")
                       , ("right" , "24px")
                       ]
  in div [ class "fixed-action-btn horizontal"
         , fabStyle
         , noTouchToSearchStyle ]
       [ a [ class "btn-floating btn-medium  waves-effect waves-light teal" ]
           [ i [ class "material-icons"
               , noTouchToSearchStyle
               , onClick address ShowForm 
               ] 
               [ text "add" ]
           ]
       ]

htmlNav : Model -> Html
htmlNav model = 
  div [ class "row indigo lighten-4" ]
        [ div [ class "col s6" ]
            [ a [ href "/" ] 
                [ img [ class "responsive-img z-depth-1"
                      , imgStyle
                      , src model.ui.imgUrl
                      ] 
                    [] 
                ]
            ]
        , div [ class "col small-text right-text right z-depth-3" ]
            [ a [ href "/" ]
                [ div [class "flow-text black-text"] [ text "Penguin's" ]
                , div [class "flow-text"] [ text "Hledger Client" ]
                ]
            ]
        ]

htmlPreloader : Model -> Html
htmlPreloader model = 
  div [ class "progress"
      , displayStyle model.ui.preloaderDisp ]
    [ div [ class "indeterminate" ]
        []
    ]

htmlError : Model -> Html
htmlError model = 
  div [ class "container"
      , id "empty-state-bear.png"
      , displayStyle model.ui.errorDisp
      ]
    [ div [ class "row" ]
        [ div [ class "col s12"]
            [ div [ class "card-panel grey lighten-5 z-depth-1" ]
                [ div [ class "row" ]
                    [ div [ class "col offset-s2 s8 center" ]
                        [ icon "material-icons large red-text" "error" ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col offset-s2 s8 center  red-text flow-text" ] 
                        [ text ("Something went wrong. The server left us on an island. "
                                  ++  "All I was told is that it's a "
                                  ++ model.errorMsg ++ "! "
                                  ++ "It's not the end of the world. Smile like I do! :)")
                        ]
                    ]
                , div [ class "row" ]
                    [ div [ class "col offset-s2 s8 center" ]
                        [ img [ class "responsive-img"
                              , src "static/images/empty-state-bear.png"
                              ]
                            []
                        ]
                    ]
                ]
            ]
        ]
    ]

viewConfirmModal : Signal.Address Action -> Model -> Html
viewConfirmModal address model =
  div [ id "confirm-modal" 
      , class "modal"
      ]
    [ div [ class "modal-content" ]
        [ text "Do you really want to delete this entry? " ]
    , div [ class "modal-footer" ]
        [ button [ class "modal-action modal-close btn waves-effect waves-light"
                 , onClick address (DeleteEntry model.entryToRemove)
                 , noTouchToSearchStyle
                 ]
            [ text "Delete" ]
        , button [ class "modal-action modal-close btn waves-effect waves-light"
                 , onClick address (UpdatedEntry (Ok "Didn't delete entry"))
                 -- ^ Makes sense to say that we updated the entry. Didn't delete.
                 , noTouchToSearchStyle
                 ]
            [ text "Cancel" ]
        ]
    ] 

     
viewForm : Signal.Address Action -> Model -> Html
viewForm address model =  
  let  fields = model.currentFields 
       (p1, p2, rest) = getPostings2 fields
       labelClass = model.ui.formLabelClass
       onInput tag = on "input" targetValue (Signal.message address << tag)
       description = div [ class "row" ]
                     [ div [ class "input-field col s12 text-left" ]
                         [ input [ id "description-field"
                                 , type' "text"
                                 , value model.currentFields.description
                                 , onInput SetDesc
                                 ]
                             []
                         , label [ for "description-field"
                                 , class labelClass
                                 ]
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
                      , label [ for "comment-field"
                              , class labelClass
                              ]
                          [ text "Comments (Optional)" ]
                      , prefixIcon "comment"
                      ]
                  ]
       account l i p tag = div [ class "input-field col s6" ]
                         [ input [ id i
                                 , type' "text"
                                 , style [ ("text-transform", "lowercase") ]
                                 -- ^ This is a hack because autocapitalize: "off/none" doesn't work!
                                 , value p.account
                                 , onInput tag
                                 ]
                                []
                            , label [ for i
                                    , class labelClass
                                    ]
                                [ text l ]
                            ] 
       amount l i p tag = div [ class "input-field col s6" ]
                            [ input [ id i
                                    , class "validate"
                                    , type' "number"
                                    , value p.amount
                                    , onInput tag
                                    ]
                                []
                            , label [ for i
                                    , class labelClass
                                    ]
                                [ text l ]
                            ] 

  in
    div [ class "container" ]
          [ div [ class "row"
                , displayStyle model.ui.formDisp
                ]
              [ Html.form [ class "col s12" ]
                  [ description
                  , comments
                  , div [ class "row" ]
                      [ account "Account #1" "acc-1" p1 SetAccountA
                      , amount  "Amount (₹)" "amount-1" p1 SetAmountA
                      ]
                  , div [ class "row" ]
                      [ account "Account #2" "acc-2" p2 SetAccountB
                      , amount "Amount (₹)" "amount-2" p2 SetAmountB
                      ]
                  , div [ class "row right" ]
                      [  button [ class "btn waves-effect waves-light teal"
                                , type' "submit"
                                , onClick address <| 
                                          if model.ui.formType == AddNewForm
                                          then AddEntry
                                          else UpdateEntry
                                , noTouchToSearchStyle
                                ]
                           [ icon "material-icons right" "send"
                           , text "Submit"
                           ]
                      ]
                  ]
              ]
          ]
                               
-- Takes Model as input for consitency
htmlFooter : Model -> Html
htmlFooter _ = footer [ class "footer" ]
               [ div [ class "footer-copyright" ]
                   [ div [ class "container" ]
                       [ div [ class "row indigo lighten-4" ]
                           [ div [ class "col m2 offset-m1 s4 z-depth-1" ]
                               [ span  []
                                   [ icon "material-icons tiny" "copyright"
                                   , a [ class "black-text"
                                       , src "#!"]
                                       [ text "Penguin" ]
                                   ]
                               ]
                           ,  div [ class "col m2 offset-m2 s4 z-depth-1" ]
                               [ span [ class "center" ]
                                   [ text "Built with "
                                   , icon "material-icons red-text tiny" "favorite"
                                   ]
                               ]
                           ]
                       ]
                   ]
               ]

viewPage : Signal.Address Action -> Model -> Html
viewPage address model = 
  div [class "container"
      , pageFlexStyle ]
  [ div [ class "divider" ]
      []
  , main' [ mainFlexStyle ]
      [ htmlNav model
      , htmlPreloader model
      , htmlError model
      , viewConfirmModal address model
      , viewForm address model
      , viewJEntryList address model
      , viewFloatingButtons address
      ]
  , htmlFooter model
  ]


-- Styling
(=>) = (,)

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


whitespacePreWrap : Attribute
whitespacePreWrap =
  style
    [ ("white-space", "pre")
    ]

-- For a sticky footer 
pageFlexStyle : Attribute
pageFlexStyle =
  style 
    [ ("display", "flex")
    , ("min-height", "100vh")
    , ("flex-direction", "column")
    ]
-- To grow main as much needed
mainFlexStyle : Attribute
mainFlexStyle =
  style 
    [ ("flex", "1 0 auto")
    ]

