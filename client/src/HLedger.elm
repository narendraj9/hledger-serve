module HLedger where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Effects, Never)

import UIComponents exposing (viewPage)
import Model exposing (..)
import HEffects exposing (..)

-- Init
init : (Model, Effects Action)
init = ( initialModel
       , fetchAll
       )

-- UPDATE
update : Action -> Model -> (Model, Effects Action)
update action model =
  let fields = model.currentFields
      (p1, p2, rest) = getPostings2 fields
      -- Funtions to avoid typing in the Action case branches         
      noEf model = (model, Effects.none)
      setUiAfterReq model = let uiStatus = model.ui
                                ui = { uiStatus
                                       | preloaderDisp = "block"
                                       , formDisp = "none"
                                       , entryListDisp = "none"
                                       , errorDisp = "none"
                                     }
                            in { model | ui = ui }
      -- Call only after a successful response [resets fields]
      setUiAfterResp model = let uiStatus = model.ui
                                 ui = { uiStatus
                                        | preloaderDisp = "none"
                                        , formDisp = "none"
                                        , entryListDisp = "block"
                                        , errorDisp = "none"
                                      }
                             in { model
                                  | ui = ui
                                  , currentFields = initialJEntry
                                }
      setUiAfterShowForm model = let uiStatus = model.ui
                                     ui = { uiStatus
                                            | formDisp = "block"
                                            , entryListDisp = "none"
                                            , preloaderDisp = "none"
                                            , errorDisp = "none"
                                          }
                                 in { model
                                      | ui = ui
                                    }
      setUiAfterError model = let uiStatus = model.ui
                                  ui = { uiStatus
                                         | preloaderDisp = "none"
                                         , formDisp = "none"
                                         , entryListDisp = "none"
                                         , errorDisp = "block"
                                       }
                              -- ^ Don't reset form fields
                              in { model | ui = ui } 
      setModelAfterResp serverEntries model =
        let uiStatus = model.ui
        in case serverEntries of
             (Just entries) -> setUiAfterResp { model | restEntries = entries }
             Nothing ->  setUiAfterError model
  in
    case action of
      -- User --> Application
      ShowForm -> noEf (setUiAfterShowForm model)
      -- Application --> Server
      AddNew -> let newEntry = model.currentFields
                in  ( setUiAfterReq model
                    , Effects.batch [ addNew newEntry
                                    , getAPenguin
                                    ]
                    )
      DeleteLast -> ( setUiAfterReq model
                    , deleteLast
                    )
      ClearAll -> ( setUiAfterReq model
                  , clearAll
                  )
      FetchAll -> ( setUiAfterReq model
                  , fetchAll
                  )

      -- Server --> Application
      AddedNew serverEntries -> noEf <| setModelAfterResp serverEntries model
      DeletedLast serverEntries -> noEf <| setModelAfterResp serverEntries model
      FetchedAll serverEntries -> noEf <| setModelAfterResp serverEntries model
      ClearedAll serverEntries -> noEf <| setModelAfterResp serverEntries model

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
                              
      (SetAmountA a1) -> let newPostings = { p1 | amount = a1 } :: p2 :: rest
                             newFields = { fields | postings = newPostings }
                       in noEf { model | currentFields = newFields }
                              
      (SetAmountB a2) -> let newPostings = p1 :: { p2 | amount = a2 } :: rest
                             newFields = { fields | postings = newPostings }
                         in noEf { model | currentFields = newFields }
    -- A new penguin gif just arrived
      (NewGif maybeUrl) -> let uiStatus = model.ui
                               newUiStatus = { uiStatus
                                               | imgUrl = Maybe.withDefault uiStatus.imgUrl maybeUrl }
                           in noEf { model
                                     | ui = newUiStatus }
                       
-- VIEW
view : Signal.Address Action -> Model -> Html
view = viewPage
