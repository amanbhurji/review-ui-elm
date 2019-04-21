module ViewPaste exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as N
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as JD
import List as L exposing ((::))
import Maybe as M
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

type Model = Failure String | Loading | Success Paste

type alias PasteId = String

type alias Paste =
  { lines : List LineWithComments
  , comments : List Comment
  , id : PasteId
  }

type alias LineWithComments =
  { line : String
  , lineComments : List Comment
  }

type alias Comment =
  { body : String
  }

type Anchor = TopLevel | Line Int

type alias Content = String

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

        Err err ->
          (Failure (Debug.toString err), Cmd.none)

onUrlChange : Url -> Msg
onUrlChange url = GotPaste (Err (BadUrl ("You cant change the url! " ++ (Url.toString url))))

onUrlRequest : UrlRequest -> Msg
onUrlRequest url = GotPaste (Err (BadUrl ("You cant request a url! " ++ (Debug.toString url))))

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
        Failure s ->
          div [] [ text ("Failed to load paste! - " ++ s) ]

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
        , tbody []
--            (viewWithComments paste.comments (viewPasteContent paste.content))
            (viewPasteBody paste.lines)
-- Add support to view top level comments
        ]
    ]

viewPasteBody : List LineWithComments -> List (Html Msg)
viewPasteBody lines =
  L.concat (L.indexedMap viewLineWithComments lines)

viewPasteContent : Content -> List (Html Msg)
viewPasteContent content = 
  (L.indexedMap viewLine (String.lines content))

viewLineWithComments : Int -> LineWithComments -> List (Html Msg)
viewLineWithComments lno lineWithComments =
  (viewLine lno lineWithComments.line) :: (L.map viewLineComment lineWithComments.lineComments)

viewLine : Int -> String -> Html Msg
viewLine lno line =
  let
    lno_str = String.fromInt lno
  in
    tr []
      [ th [] [ a [ href ("#" ++ lno_str) ] [ text lno_str ] ]
      , td [] [ text line ]
      ]

viewLineComment : Comment -> Html Msg
viewLineComment comment =
  tr [ class "bg-info" ]
    [ td [ colspan 2 ]
        [ div []
            [ text comment.body ]
        ]
    ]

{-
viewLine : Int -> String -> Html Msg
viewLine lno line =
  tr []
    [ th [] [ text (String.fromInt lno) ]
    , td [] [ text line ]
    ]

viewWithComments : List Comment -> List (Html Msg) -> List (Html Msg)
viewWithComments comments contentrows =
  L.foldl viewLineComment contentrows comments

viewLineComment : Comment -> List (Html Msg) -> List (Html Msg)
viewLineComment comment contentrows =
  let
    commentrow =
      tr [ class "bg-info" ]
        [ td [ colspan 2 ]
            [ div []
                [ text comment.body ]
            ]
        ]

    maybeCommentNum = lineNumForComment comment

    -- this logic to insert comments is wrong
    maybeNewContentRows =
      M.map (\i -> (L.take i contentrows) ++ [commentrow] ++
        (L.drop ((L.length contentrows) - i + 1) contentrows)) maybeCommentNum
  in
    M.withDefault contentrows maybeNewContentRows

lineNumForComment : Comment -> Maybe Int
lineNumForComment comment =
  lineNumForAnchor comment.anchor
-}

lineNumForAnchor : Anchor -> Maybe Int
lineNumForAnchor anchor =
  case anchor of
    TopLevel  -> Nothing
    Line i    -> Just i

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
    (JD.field "lines" (JD.list lineDecoder))
    (JD.field "comments" (JD.list commentDecoder))
    (JD.field "id" JD.string)

lineDecoder : JD.Decoder LineWithComments
lineDecoder =
  JD.map2 LineWithComments
    (JD.field "line" JD.string)
    (JD.field "comments" (JD.list commentDecoder))

commentDecoder : JD.Decoder Comment
commentDecoder =
  JD.map Comment
    (JD.field "body" JD.string)

{-
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
-}

getPasteIdFromUrl : Url -> PasteId
getPasteIdFromUrl url = dropLeft 4 (Maybe.withDefault "failedToParseQuery" url.query)

