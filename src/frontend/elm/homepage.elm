module Homepage where

import Debug exposing (..)
import Text as T
import Window as W
import Signal
import Signal exposing (Mailbox)
import String exposing (isEmpty, join)
import Http exposing (..)
import Task exposing (Task, andThen)
import Json.Decode as Decode exposing (customDecoder, decodeString
                                      , object2, object4, maybe
                                      , string , map, list)
import Json.Decode as Decode exposing (Decoder, (:=))
import Json.Encode as Encode
import List
import Keyboard
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Time


-- MODELS
type alias Service = String

type alias Urls = { full    : Maybe String
                  , preview : Maybe String
                  }

type alias Item = { title   : String
                  , album   : String
                  , artists : List String
                  , urls    : Urls
                  }

type alias Fragment = { service  : Service
                      , items    : List Item
                      }

emptyUrls : Urls
emptyUrls = { full = Nothing
            , preview = Nothing
            }

emptyItem : Item
emptyItem = { title  = ""
            , album  = ""
            , artists = []
            , urls   = emptyUrls
            }

type alias Request =
    { searchTerm   : String
    , sendAttempts : Int
    }

type Update = Term String
            | Search
            | NoOp

emptyRequest : Request
emptyRequest = { searchTerm   = ""
               , sendAttempts = 0
               }

update : Update -> Request -> Request
update u m =
    case u of
      Term content -> { m | searchTerm <- content }
      Search       -> { m | sendAttempts <- m.sendAttempts + 1 }

type alias FilePath = String

-- VIEW STYLING
styleContent : Attribute
styleContent =
    Attr.style [ ("padding", "16px")
               , ("margin-left", "auto")
               , ("margin-right", "auto")
               , ("width", "400px")
               ]

styleItemList : Attribute
styleItemList =
    Attr.style [ ("list-style-type", "none") ]


-- VIEWS
view : (Int, Int) -> Request -> List Fragment -> Html
view (h, w) model fragments =
    let resultsView = case fragments of
                        [] -> div [ Attr.class "empty-space" ] []
                        _  -> div [ Attr.class "results" ] (List.map3 viewFragment [ (h, w), (h, w) ] [ model.searchTerm, model.searchTerm ] fragments)
    in div [ Attr.class "content"
           , styleContent
           ]
           [ inputView (h, w)
           , resultsView
           ]

-- keycode "enter"
is13 : Int -> Result String ()
is13 code = if code == 13 then Ok () else Err "keycode is not enter"

inputView : (Int, Int) -> Html
inputView (h, w) =
    let inputWidth = (toFloat w) / 3
    in
      div [] [ input [ Attr.style [ ("width", "100%") ]
                     , Attr.type' "text"
                     , Attr.placeholder "Enter track name…"
                     , Attr.autofocus True
                     , on "keyup" (customDecoder keyCode is13) (always <| Signal.message updateMailbox.address Search)
                     --, onKeyUp (\ code -> if log "key code - " code == 13 then send updateMailbox <| Search else send updateMailbox <| NoOp)
                     , on "input" targetValue (Signal.message updateMailbox.address << Term)
                     ] []
             , button [ Attr.style [ ("margin-top", "8px") ]
                      , onClick updateMailbox.address Search
                      ] [ text "Search" ]
             ]

viewFragment : (Int, Int) -> String -> Fragment -> Html
viewFragment (h, w) searchTerm f =
    let itemList =
            if List.isEmpty f.items
            then [ h2 [ Attr.class "header2" ] [ text <| noResults f.service searchTerm ] ]
            else [ viewList f ]
    in div [ Attr.class "fact" ] <| viewServiceLogo f.service :: itemList

viewList : Fragment -> Html
viewList f =
    ol [ Attr.class "item-collection" ]
       <| (List.map (viewItem f.service) f.items) ++ [ div [ Attr.class "fact-space" ] [] ]

viewServiceLogo : Service -> Html
viewServiceLogo provider =
    img [ Attr.class "provider-logo"
        , Attr.src (toLogo provider)
        , Attr.alt (toString provider)
        , Attr.width 360
        , Attr.height 140
        ] []

viewItem : Service -> Item -> Html
viewItem provider item =
    li [ Attr.class "item", styleItemList ]
       [ h2 [ Attr.class "header2" ] [ text (viewItemTitle item) ]
       , viewUrls (log "provider" provider) item.urls
       ]

viewItemTitle : Item -> String
viewItemTitle item =
    item.title ++ " by " ++ (join ", " item.artists)

viewUrls : Service -> Urls -> Html
viewUrls provider urls =
    case provider of
      "Spotify" ->
          viewSpotify urls

      "Soundcloud" ->
          viewSoundcloud urls

      _ ->
          div [] []

viewSoundcloud : Urls -> Html
viewSoundcloud urls =
    div [ Attr.class "item-urls" ]
        [ case urls.full of
            Nothing  ->
                div [] []

            Just url ->
                div [ Attr.class "item-url-full" ]
                    [ iframe [ Attr.class "audio-full"
                             , Attr.src <| "https://w.soundcloud.com/player/?url=" ++ url
                             , Attr.style [ ("width", "100%") ]
                             , Attr.height 166
                             , Attr.property "framborder" (Encode.string "0")
                             ] []
                    ]
        ]

viewSpotify : Urls -> Html
viewSpotify urls =
    div [ Attr.class "item-urls" ]
        [ case urls.full of
            Nothing  ->
                case urls.preview of
                  Nothing ->
                      div [] []

                  Just p  ->
                      div [] [ div [ Attr.class "item-url-preview" ]
                                   [ audio [ Attr.class "audio-preview"
                                           , Attr.src p
                                           , Attr.autoplay False
                                           , Attr.controls True
                                           ] []
                                   ]
                             , h6 [] [ text trackNotAvailable ]
                             ]
            -- https://developer.spotify.com/technologies/widgets/spotify-play-button/
            Just url ->
                div [ Attr.class "item-url-full" ]
                    [ iframe [ Attr.class "audio-full"
                             , Attr.src <| "https://embed.spotify.com/?uri=" ++ url
                             , Attr.width 300
                             , Attr.height 80
                             , Attr.property "allowtransparency" (Encode.string "true")
                             , Attr.property "framborder" (Encode.string "0")
                             ] []
                    ]
        ]


toLogo : Service -> FilePath
toLogo dp =
    case dp of
      "Spotify" -> "../../../assets/spotify.jpeg"
      _       -> "" -- TODO: provide default img

trackNotAvailable : String
trackNotAvailable =
    "(Not available in your country. You can just listen to a preview)"

noResults : String -> String -> String
noResults service searchTerm =
    ("No results on " ++ service ++ " for: \'" ++ searchTerm ++ "\'")
-- UPDATE
updateMailbox : Mailbox Update
updateMailbox =
    Signal.mailbox NoOp

model : Signal Request
model =
    updateMailbox.signal |> Signal.foldp update emptyRequest

results : Mailbox (List Fragment)
results =
    Signal.mailbox []

port runner : Signal (Task Http.Error ())
port runner =
    Signal.map2 toUrl updateMailbox.signal model
        |> Signal.filter (not << isEmpty) ""
        |> Signal.dropRepeats
        |> Signal.map (\url -> Http.get resultsList url)
        |> Signal.map (\task -> task `andThen` Signal.send results.address)

-- HTTP
toUrl : Update -> Request -> String
toUrl action m =
    let url = Http.url "http://localhost:3000" [ ("term", m.searchTerm) ]
    in case action of
         Search -> url
         _      -> ""

handleError : Http.Error -> Result String (List Fragment)
handleError err =
    case err of
      BadResponse code msg  ->
          Err ("Error: " ++ (toString code) ++ " " ++ msg)
      UnexpectedPayload str ->
          Err "Json decoding failed"
      _                     ->
          Err "Couldn't get results at this time. Please retry later."

-- JSON
urls : Decoder Urls
urls =
     object2 Urls (maybe ("full" := Decode.string)) (maybe ("preview" := Decode.string))

item : Decoder Item
item =
    object4 Item ("title" := Decode.string) ("album" := Decode.string)
                 ("artists" := (list Decode.string)) ("urls" := urls)

fragment : Decoder Fragment
fragment =
    object2 Fragment ("service" := Decode.string) ("items" := list item)

resultsList : Decoder (List Fragment)
resultsList =
    "results" := (list fragment)


main : Signal Html
main =
    Signal.map3 view W.dimensions model results.signal
