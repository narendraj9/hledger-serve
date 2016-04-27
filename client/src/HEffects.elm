module HEffects ( fetchAll
                , addEntry
                , updateEntry
                , deleteEntry
                , getAPenguin
                , openModal
                , modalMailbox
                , showToast
                , toastMailbox
                ) where
                
import Http 
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=), succeed)
import Json.Encode as JsonEn exposing (string, list, Value)
import Task exposing (Task, andThen)

import Model exposing (..)
import UIComponents exposing ((=>))


-- | A mailbox for opening up a modal view with id #confirm-modal | TOFIX
modalMailbox : Signal.Mailbox String
modalMailbox = Signal.mailbox "#confirm-modal"


-- | A mailbox for showing toasts to the user
toastMailbox : Signal.Mailbox String
toastMailbox = Signal.mailbox ""


-- | Open open a modal with a given id
openModal : String -> Effects Action
openModal modalId = Signal.send modalMailbox.address modalId
                  |> Task.map NoOp
                  |> Effects.task


-- | Show a toast to the user
showToast : String -> Effects Action
showToast text = Signal.send toastMailbox.address text
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
fetchAll = Http.send Http.defaultSettings
                { verb = "GET"
                , url = serviceUri ++ "/entry"
                , headers = [ ("content-type", "application/json") ]         
                , body = Http.empty
                }
         |> Http.fromJson decodeJEntryList
         |> Task.toResult
         |> Task.map FetchedAll
         |> Effects.task


-- | POST /entry 
addEntry : JEntry -> Effects Action
addEntry jentry = Http.send Http.defaultSettings
                { verb = "POST"
                , url = serviceUri ++ "/entry"
                , headers = [ ("content-type", "application/json") ]         
                , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                }
              |> Task.map (\_ -> "AddedEntry")
              |> Task.toResult
              |> Task.map AddedEntry
              |> Effects.task


-- | PUT /entry
updateEntry : JEntry -> Effects Action
updateEntry jentry = Http.send Http.defaultSettings
                     { verb = "PUT"
                     , url = serviceUri ++ "/entry"
                     , headers = [ ("content-type", "application/json") ]         
                     , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                     }
                   |> Task.map (\_ -> "UpdatedEntry")
                   |> Task.toResult
                   |> Task.map UpdatedEntry
                   |> Effects.task


-- | DELETE /entry
deleteEntry : JEntry -> Effects Action
deleteEntry jentry = Http.send Http.defaultSettings
                     { verb = "DELETE"
                     , url = Http.url (serviceUri ++ "/entry")
                             [("entry_number", toString jentry.number)]
                     , headers = [ ("content-type", "application/json") ]         
                     , body = Http.string (JsonEn.encode 0 <| encodeJEntry jentry)
                     }
                   |> Task.map (\_ -> "DeletedEntry")
                   |> Task.toResult
                   |> Task.map DeletedEntry
                   |> Effects.task


-- | Gets an random penguin image url from giphy
getAPenguin : Effects Action
getAPenguin = getRandomGif "cute penguin"

