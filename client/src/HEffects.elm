module HEffects ( fetchAll
                , addNew
                , updateEntry
                , deleteEntry
                , deleteLast
                , clearAll
                , getAPenguin
                , openModal
                , modalMailbox
                ) where
                
import Http 
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Json.Encode as JsonEn exposing (string, list, Value)
import Task exposing (Task, andThen)

import Model exposing (..)
import UIComponents exposing ((=>))


-- | A mailbox for opening up a modal view with id #confirm-modal | TOFIX
modalMailbox : Signal.Mailbox String
modalMailbox = Signal.mailbox "#confirm-modal"


-- | Open open a modal with a given id
openModal : String -> Effects Action
openModal modalId = Signal.send modalMailbox.address modalId
                  |> Task.map NoOp
                  |> Effects.task


-- | Service info
serviceUri : String
serviceUri = "http://services.vicarie.in" 

-- Auxiliary functions [For fetching gifs]
getRandomGif : String -> Effects Action
getRandomGif topic = Http.get decodeUrl (randomUrl topic)
                   |> Task.toMaybe
                   |> Task.map NewGif
                   |> Effects.task

decodeUrl : Json.Decoder String
decodeUrl = Json.at ["data", "fixed_height_small_url"] Json.string

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
decodeJEntry =  Json.object5 JEntry ("number" := Json.int)
                                    ("date" := Json.string)
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
encodeJEntry jentry = JsonEn.object [ ("number", JsonEn.int jentry.number)
                                    , ("date", string jentry.date)
                                    , ("description", string jentry.description)
                                    , ("comment", string jentry.comment)
                                    , ("postings", JsonEn.list
                                         (List.map encodePosting jentry.postings))
                                    ]

fetchAll : Effects Action
fetchAll = Http.get decodeJEntryList (serviceUri ++ "/entry")
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
            
updateEntry : JEntry -> Effects Action
updateEntry jentry = Http.send Http.defaultSettings
                     { verb = "PUT"
                     , url = serviceUri ++ "/entry"
                     , headers = [ ("content-type", "application/json") ]         
                     , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                     }
                   |> Http.fromJson decodeJEntryList
                   |> Task.toMaybe
                   |> Task.map UpdatedEntry
                   |> Effects.task

deleteEntry : JEntry -> Effects Action
deleteEntry jentry = Http.send Http.defaultSettings
                     { verb = "DELETE"
                     , url = serviceUri ++ "/entry"
                     , headers = [ ("content-type", "application/json") ]         
                     , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                     }
                   |> Http.fromJson decodeJEntryList
                   |> Task.toMaybe
                   |> Task.map DeletedEntry
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

