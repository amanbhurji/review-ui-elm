module ViewPaste exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string, map2)

main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- Model

type Model = PasteId | Failure | Loading | Success Paste

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

init : () -> (Model, Cmd Msg)
init _ =
  ( Loading
  , getPasteFromServer
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

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- View

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

pasteId = "447af5e7-5caf-4459-87cf-9deaef99a5bd"

getPasteFromServer : Cmd Msg
getPasteFromServer =
  Http.get 
    { url = "http://localhost:8081/paste/" ++ pasteId
    , expect = Http.expectJson GotPaste pasteDecoder
    }

pasteDecoder : Decoder Paste
pasteDecoder =
  map2 Paste
    (field "content" string)
    (field "id" string)
