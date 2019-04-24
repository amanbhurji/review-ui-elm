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

type Model = Failure String | Loading | Success Root

type alias PasteId = String

type alias Root =
  { paste : Paste
  , newComment : Maybe Comment
  , createNewComment : Maybe Anchor
  }

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

type Msg
  = GotPaste (Result Http.Error Paste)
  | NewComment Anchor
  | SetCommentData String
  | MkComment

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotPaste result ->
      case result of
        Ok paste ->
          ( Success
              { paste = paste
              , newComment = Nothing
              , createNewComment = Nothing
              }
          , Cmd.none
          )

        Err err ->
          (Failure (Debug.toString err), Cmd.none)

    NewComment anchor ->
      case anchor of
        TopLevel ->
          Debug.todo "ok"

        Line lno ->
          case model of
            Success root ->
              ( Success { root | createNewComment = Just anchor }
              , Cmd.none
              )
            _ -> Debug.todo "todo throw error"

    SetCommentData s ->
      case model of
        Success root ->
          ( Success { root | newComment = Just { body = s } }
          , Cmd.none
          )
        _ -> Debug.todo "todo throw error"

    MkComment ->
      case model of
        Success root ->
          ( Success
              { root |
                  createNewComment = Nothing,
                  newComment = Nothing
              }
          , postComment root
          )
        _ -> Debug.todo "todo throw error"

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

        Success root ->
          viewPaste root
    ]

viewPaste : Root -> Html Msg
viewPaste root =
  div [ id "paste_content", class "table-responsive" ]
    [ table [ class "table table-dark table-sm", style "width" "100%" ]
        [ thead []
            [ tr []
                [ th [] [ text "#" ]
                , th [] [ text "Paste" ]
                ]
            ]
        , tbody []
            (viewPasteBody root)
-- Add support to view top level comments
        ]
    ]

viewPasteBody : Root -> List (Html Msg)
viewPasteBody root =
  let
    f = viewLineWithComments root.createNewComment root.newComment
  in
    L.concat (L.indexedMap f root.paste.lines)

viewLineWithComments :
  Maybe Anchor ->
  Maybe Comment ->
  Int ->
  LineWithComments ->
  List (Html Msg)
viewLineWithComments maybeAnchor maybeComment lno lineWithComments =
  (viewLine lno lineWithComments.line)
    :: (viewNewComment lno maybeAnchor maybeComment)
    ++ (L.map viewLineComment lineWithComments.lineComments)

viewLine : Int -> String -> Html Msg
viewLine lno line =
  let
    lno_str = String.fromInt lno
  in
    tr []
      [ th [] [ a [ href ("#" ++ lno_str) ] [ text lno_str ] ]
      , td [] [ text line ]
      ]

-- returning List coz too lazy to handle returning maybe
viewNewComment : Int -> Maybe Anchor -> Maybe Comment -> List (Html Msg)
viewNewComment lno maybeAnchor maybeComment =
  let
    anchorLine = M.andThen lineNumForAnchor maybeAnchor
  in
    if anchorLine == (Just lno) then
      [ viewCommentTextBox maybeComment ]
    else
      []

viewCommentTextBox : Maybe Comment -> Html Msg
viewCommentTextBox maybeComment =
  case maybeComment of
    Nothing ->
      Debug.todo "todo create empty new comment html"
    Just comment ->
      Debug.todo "todo create populated new"

viewLineComment : Comment -> Html Msg
viewLineComment comment =
  tr [ class "bg-info" ]
    [ td [ colspan 2 ]
        [ div []
            [ text comment.body ]
        ]
    ]

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

postComment : Root -> Cmd Msg
postComment = Debug.todo "todo4"

-- Decoders

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

