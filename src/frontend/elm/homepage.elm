module Homepage where

import Debug (..)
import Text as T
import Window as W
import Signal as S
import String (isEmpty, join)
import Http
import Json.Decode (Decoder, customDecoder, decodeString
                   , object2, object4, maybe, (:=)
                   , string , map, list)
import Json.Encode as Encode
import List
import Keyboard
import Html (..)
import Html.Attributes as Attr
import Html.Events (..)
import Html.Lazy (lazy)
import Time


-- MODELS
type alias Service = String

type alias Urls = { full    : Maybe String
                  , preview : String
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
            , preview = ""
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
view : (Int, Int) -> Request -> Maybe (List Fragment) -> Html
view (h, w) model resM =
    let results = case resM of
                    Nothing  -> div [ Attr.class "empty-space" ] []
                    Just res -> div [ Attr.class "results" ] (List.map3 viewFragment [(h, w) ] [ model.searchTerm ] res)
    in div [ Attr.class "content"
           , styleContent
           ]
           [ inputView (h, w)
           , results
           ]

-- keycode "enter"
is13 : Int -> Result String ()
is13 code = if code == 13 then Ok () else Err "keycode is not enter"

inputView : (Int, Int) -> Html
inputView (h, w) =
    let inputWidth = log "resize" (toFloat w) / 3
    in
      div [] [ input [ Attr.style [ ("width", "100%") ]
                     , Attr.type' "text"
                     , Attr.placeholder "Enter track name…"
                     , Attr.autofocus True
                     , on "keyup" (customDecoder keyCode is13) (always <| S.send updateCh Search)
                     --, onKeyUp (\ code -> if log "key code - " code == 13 then send updateCh <| Search else send updateCh <| NoOp)
                     , on "input" targetValue (S.send updateCh << Term)
                     ] []
             , button [ Attr.style [ ("margin-top", "8px") ]
                      , onClick (S.send updateCh Search)
                      ] [ text "Search" ]
             ]

viewFragment : (Int, Int) -> String -> Fragment -> Html
viewFragment (h, w) searchTerm f =
    let itemList =
            if List.isEmpty f.items
            then [ h2 [ Attr.class "header2" ] [ text ("No results for: \'" ++ searchTerm ++ "\'") ] ]
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
       , viewUrls provider item.urls
       ]

viewItemTitle : Item -> String
viewItemTitle item =
    item.title ++ " by " ++ (join ", " item.artists)

viewUrls : Service -> Urls -> Html
viewUrls provider urls =
    case provider of
      "Spotify" ->
          div [ Attr.class "item-urls" ]
              [
                case urls.full of
                  Nothing  ->
                      if (not << isEmpty) urls.preview
                      then div []
                               [ div [ Attr.class "item-url-preview" ]
                                     [ audio [ Attr.class "audio-preview"
                                             , Attr.src urls.preview
                                             , Attr.autoplay False
                                             , Attr.controls True
                                             ] []
                                     ]
                                , h6 [] [ text "(Preview only)" ]
                               ]
                      else div [] []
                  -- https://developer.spotify.com/technologies/widgets/spotify-play-button/
                  Just url -> div [ Attr.class "item-url-full" ]
                                  [ iframe [ Attr.class "audio-full"
                                           , Attr.src <| "https://embed.spotify.com/?uri=" ++ url
                                           , Attr.width 300
                                           , Attr.height 80
                                           , Attr.property "allowtransparency" (Encode.string "true")
                                           , Attr.property "framborder" (Encode.string "0")
                                           ] []
                                  ]
              ]

      _       -> div [] []


toLogo : Service -> FilePath
toLogo dp =
    case dp of
      "Spotify" -> "../../assets/spotify.jpeg"
      _       -> "" -- TODO: provide default img


-- UPDATE
updateCh : S.Channel Update
updateCh = S.channel NoOp

model : Signal Request
model = S.subscribe updateCh |> S.foldp update emptyRequest

results : Signal (Maybe (List Fragment))
results =
    S.map2 toUrl (S.subscribe updateCh) model
    --toUrl <~ (subscribe updateCh) ~ model
        --|> sampleOn (Time.every Time.second)
        |> S.keepIf (not << isEmpty) ""
        |> S.dropRepeats
        |> S.map toHttpRequest
        |> Http.send
        |> S.map toResults
        |> S.map (log "toResults")


-- HTTP
toHttpRequest : String -> Http.Request String
toHttpRequest url = Http.get url

toUrl : Update -> Request -> String
toUrl u m =
    let url = "http://localhost:3000/?term=" ++ m.searchTerm
    in case u of
      Search -> log "Search: " url
      _      -> ""

toResults : Http.Response String -> Maybe (List Fragment)
toResults response =
    case decodeResponse resultsList response of
      Ok v -> Just v
      _    -> Nothing


-- JSON
decodeResponse : Decoder (List Fragment) -> Http.Response String -> Result String (List Fragment)
decodeResponse decoder response =
    case response of
      Http.Failure code msg -> Err <| "Something went wrong (code: " ++ toString code ++ ", message: " ++ msg ++ ")"
      Http.Success json     -> log "decodedResponse" <| decodeString resultsList json
      _                     -> Err "Something's wrong and I don't know what exactly"

urls : Decoder Urls
urls =
     object2 Urls (maybe ("full" := string)) ("preview" := string)

item : Decoder Item
item =
    object4 Item ("title" := string) ("album" := string)
                 ("artists" := (list string)) ("urls" := urls)

fragment : Decoder Fragment
fragment =
    object2 Fragment ("service" := string) ("items" := list item)

resultsList : Decoder (List Fragment)
resultsList =
    "results" := (list fragment)


main : Signal Html
main =
    S.map3 view W.dimensions model results
