module Model where

import Http as Http exposing (Error)

-- Model definitions
type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { number : Int
                    , date : String
                    , description : String
                    , comment : String
                    , postings : List Posting
                    }
type FormType = UpdateForm | AddNewForm 
type alias UiStatus = { imgUrl : String
                      , preloaderDisp : String
                      , formDisp : String
                      , entryListDisp : String
                      , errorDisp : String
                      , formType : FormType
                      , formLabelClass : String
                      }
type alias Model = { currentFields : JEntry
                   , restEntries: List JEntry
                   , entryToRemove : JEntry
                   , errorMsg : String
                   , ui : UiStatus
                   }


initialPostings : List Posting
initialPostings = [ { account =  "", amount = ""}
                  , { account = "", amount = ""}
                  ]

initialJEntry : JEntry
initialJEntry = { date = ""
                , number = 0
                , description = ""
                , comment = ""
                , postings = initialPostings
                }

initialUiStatus : UiStatus
initialUiStatus = { imgUrl = "static/images/penguin.png"
                  , preloaderDisp = "block"
                  , formDisp = "none"
                  , formType = AddNewForm
                  , entryListDisp = "none"
                  , errorDisp = "none"
                  , formLabelClass = ""
                  }
  
initialModel : Model
initialModel = { currentFields = initialJEntry
               , restEntries = []
               , entryToRemove = initialJEntry
               , errorMsg = ""
               , ui = initialUiStatus
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


-- The Action data type can be thought of as part of the overall model
type Action = ShowForm
            | NoOp ()
            | NewGif (Maybe String)
            | AddEntry
            | FetchAll
            | SetEntryToRemove JEntry
            | DeleteEntry JEntry
            | SetDesc String
            | SetComment String
            | SetAccountA String
            | SetAccountB String
            | SetAmountA String
            | SetAmountB String
            | EditEntry JEntry
            | FetchedAll (Result Http.Error (List JEntry))
            -- ^ Http.Error is an inconsistency! [Because of Http.fromJson]
            | AddedEntry (Result Http.RawError String)
            | DeletedEntry (Result Http.RawError String)
            | UpdatedEntry (Result Http.RawError String)
            | UpdateEntry 


