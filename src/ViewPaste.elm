module ViewPaste exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as N
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode exposing (Decoder, field, string, map2)
import String exposing (dropLeft)
import Url exposing (Url)

main =
  Browser.application
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = viewDoc
  , onUrlChange = onUrlChange
  , onUrlRequest = onUrlRequest
  }

-- Model

type Model = Failure | Loading | Success Paste

type alias PasteId = String

type alias Paste =
  { content : Content
--  , comments : List Comment
  , id : String
  }

type alias Content = String

type alias Comment =
  { body : String
  , anchor : Anchor
  }

type Anchor = TopLevel | Line Int

init : () -> Url -> N.Key -> (Model, Cmd Msg)
init _ url _ =
  ( Loading
  , getPasteFromServer (getPasteIdFromUrl url)
  )

onUrlChange : Url -> Msg
onUrlChange _ = GotPaste (Err (BadUrl "lmaolmao"))

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ = GotPaste (Err (BadUrl "lelel"))


-- Update

type Msg = GotPaste (Result Http.Error Paste)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPaste result ->
      case result of
        Ok paste ->
          (Success paste, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- View

viewDoc : Model -> Document Msg
viewDoc model =
  { title = "View paste"
  , body = [ view model ]
  }

view : Model -> Html Msg
view model = 
  div [ class "container" ]
    [ case model of
        Failure ->
          div [] [ text "Failed to load paste!" ]

        Loading ->
          div [] [ text "Loading ..." ]

        Success paste ->
          viewPaste paste
    ]

viewPaste : Paste -> Html Msg
viewPaste paste = 
  div [ id "paste_content", class "table-responsive" ]
    [ table [ class "table table-dark table-sm", style "width" "100%" ]
        [ thead []
            [ tr []
                [ th [] [ text "#" ]
                , th [] [ text "Paste" ]
                ]
            ]
        , viewPasteContent paste.content
        ]
    ]

viewPasteContent : Content -> Html Msg
viewPasteContent content = 
  tbody []
    (List.indexedMap viewLine (String.lines content))

viewLine : Int -> String -> Html Msg
viewLine lno line = 
  tr []
    [ th [] [ text (String.fromInt lno) ]
    , td [] [ text line ]
    ]

-- Http

getPasteFromServer : PasteId -> Cmd Msg
getPasteFromServer pasteId =
  Http.get
    { url = "http://localhost:8081/paste/" ++ pasteId
    , expect = Http.expectJson GotPaste pasteDecoder
    }

pasteDecoder : Decoder Paste
pasteDecoder =
  map2 Paste
    (field "content" string)
    (field "id" string)

getPasteIdFromUrl : Url -> PasteId
getPasteIdFromUrl url = dropLeft 4 (Maybe.withDefault "failedToParseQuery" url.query)

