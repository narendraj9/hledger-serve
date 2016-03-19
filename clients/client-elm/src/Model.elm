module Model where

-- Model definitions
type alias Posting = { account : String
                     , amount : String
                     }
type alias JEntry = { date : String
                    , description : String
                    , comment : String
                    , postings : List Posting
                    }
type alias UiStatus = { imgUrl : String
                      , preloaderDisp : String
                      , formDisp : String
                      , entryListDisp : String
                      , errorDisp : String
                      }
type alias Model = { currentFields : JEntry
                   , restEntries: List JEntry
                   , ui : UiStatus
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

initialUiStatus : UiStatus
initialUiStatus = { imgUrl = "_assets/penguin.png"
                  , preloaderDisp = "none"
                  , formDisp = "none"
                  , entryListDisp = "block"
                  , errorDisp = "none"
                  }
  
initialModel : Model
initialModel = { currentFields = initialJEntry
               , restEntries = []
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
            | AddNew
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

