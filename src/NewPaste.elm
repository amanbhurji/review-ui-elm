module NewPaste exposing (main)

import Browser
import Browser.Navigation as N
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode
import Json.Encode as E
import String exposing (slice)

main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

-- Model

type alias Model = Maybe String

init : () -> (Model, Cmd Msg)
init _ = (Nothing, Cmd.none)

-- Update

type Msg = MkReview | SetData String | GotPasteId (Result Http.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetData s ->
      (Just s, Cmd.none)
    MkReview ->
      case model of
        Just s ->
          (model, postReview s)
        Nothing ->
          (Just "Error add some data", Cmd.none)
    GotPasteId result ->
      case result of 
        Ok pasteId ->
          (Just pasteId, N.load ("/src/ViewPaste.elm?pid=" ++ (slice 1 -1 pasteId)))
        Err _ ->
          (Just "Error posting data", Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

-- View

view : Model -> Html Msg
view model =
  div [ class "container-fluid h-100 bg-warning" ]
    [ div [ class "d-flex flex-column h-100 bg-danger p-4" ]
        [ div [ class "row flex-grow-1 bg-dark" ]
            [ textarea [ tabindex 0, spellcheck False, class "text-white", onInput SetData, style "background" "0 0", style "width" "100%", style "height" "100%", style "border" "0"] []
            ]
        , div [ class "row controls mt-4 bg-success" ]
            [ button [ class "btn btn-secondary", onClick MkReview ] [ text "Save" ] ]
        ]
    ]

-- Http

postReview : String -> Cmd Msg
postReview s =
  Http.post
    { url = "http://localhost:8081/paste"
    , body = Http.stringBody "application/json" (E.encode 0 (E.string s))
    , expect = Http.expectString GotPasteId
    }

