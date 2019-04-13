module ViewPaste exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as N
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as JD
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
  , comments : List Comment
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

onUrlChange : Url -> Msg
onUrlChange _ = GotPaste (Err (BadUrl "lmaolmao"))

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ = GotPaste (Err (BadUrl "lelel"))

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

pasteDecoder : JD.Decoder Paste
pasteDecoder =
  JD.map3 Paste
    (JD.field "content" JD.string)
    (JD.field "comments" (JD.list commentDecoder))
    (JD.field "id" JD.string)

commentDecoder : JD.Decoder Comment
commentDecoder =
  JD.map2 Comment
    (JD.field "body" JD.string)
    (JD.field "anchor" anchorDecoder)

anchorDecoder : JD.Decoder Anchor
anchorDecoder =
  JD.oneOf [topLevelDecoder, lineDecoder]

topLevelDecoder : JD.Decoder Anchor
topLevelDecoder =
  JD.andThen (\str ->
    case str of
      "toplevel"  -> JD.succeed TopLevel
      unknown     ->
        JD.fail ("Unexpected value decoding Anchor(TopLevel): " ++ unknown)
    ) JD.string

lineDecoder : JD.Decoder Anchor
lineDecoder =
  JD.map Line (JD.field "line" JD.int)

getPasteIdFromUrl : Url -> PasteId
getPasteIdFromUrl url = dropLeft 4 (Maybe.withDefault "failedToParseQuery" url.query)

