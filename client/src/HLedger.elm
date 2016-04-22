module HLedger where

import String
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Effects exposing (Effects, Never)
import Task exposing (succeed)

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

      -- | State of ui after a request is made to the server
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
                                        , formType = AddNewForm
                                        , formLabelClass = ""
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
      afterResponse result =
        case result of
          (Ok serverEntries) ->
            let model' = setUiAfterResp model
                newModel = { model' | restEntries = serverEntries }
            in ( newModel
               , Effects.none
               )
          (Err error) ->
            ( setUiAfterError { model | errorMsg = toString error }
            , Effects.none
            )
  in
    case action of
      NoOp _ -> noEf model
      -- User --> Application
      ShowForm -> 
        noEf (setUiAfterShowForm model)

      EditEntry entry -> 
        let uiStatus = model.ui
            uiStatus' = { uiStatus | formLabelClass = "active"
                        , formType = UpdateForm
                        }
            newModel = { model | currentFields = entry
                       , ui = uiStatus'
                       }
        in
          (newModel, Effects.task (succeed ShowForm))

      SetEntryToRemove entry -> 
        ( { model | entryToRemove = entry }
        , openModal "#confirm-modal"
        )

      -- Application --> Server
      AddEntry -> 
        let newEntry = model.currentFields
        in  ( setUiAfterReq model
            , Effects.batch [ addEntry newEntry
                            , getAPenguin
                            ]
            )

      UpdateEntry -> 
        ( setUiAfterReq model
        , updateEntry model.currentFields
        )

      FetchAll -> 
        ( setUiAfterReq model
        , fetchAll
        )

      DeleteEntry entry -> 
        ( setUiAfterReq model
        , deleteEntry entry
        )

      -- Server --> Application
      FetchedAll result -> afterResponse result
      
      AddedEntry result -> afterResponse result
      UpdatedEntry result -> afterResponse result
      DeletedEntry result -> afterResponse result
      -- ^ This sections sucks! I know.

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
