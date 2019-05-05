module ViewPaste exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as JD
import Json.Encode as JE
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
  , onUrlRequest = ClickedLink
  }

-- Model

type Model = Failure String | Loading | Success Root

type alias PasteId = String

type alias Root =
  { paste : Paste
  , newComment : Maybe Comment
  , commentAnchor : Maybe Anchor
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

-- add support for top level comments
type Anchor = Line Int

type alias Content = String

moveCommentToPaste : Root -> Root
moveCommentToPaste root =
  let
    get xs n = L.head (L.drop n xs)
    replace xs newN n = L.take n xs ++ newN :: L.drop (n+1) xs
    lno = M.map lineNumForAnchor root.commentAnchor
    lineToUpdate = M.andThen (get root.paste.lines) lno
    newCommentAsList = M.withDefault [] (M.map L.singleton root.newComment)
    updatedLine = M.map
      ( \x -> { x | lineComments = newCommentAsList ++ x.lineComments } )
      lineToUpdate
    printRoot = Debug.log "root" root
    printAnchor = Debug.log "lno" lno
    printOgLine = Debug.log "ogLine" lineToUpdate
    printUpdatedLine = Debug.log "updatedLine" updatedLine
    ogPaste = root.paste
    updatedPaste =
      M.andThen ( \l ->
        M.map
          ( \n ->
            { ogPaste | lines = replace ogPaste.lines l n }
          )
          lno
      ) updatedLine
    newOrOg = M.withDefault ogPaste updatedPaste
  in
    { root |
        paste = newOrOg,
        commentAnchor = Nothing,
        newComment = Nothing
    }

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
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
  | ClickedLink UrlRequest
  | Commented (Result Http.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
-- refactor - case ( msg, model ) of
  case msg of
    ClickedLink urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          case url.fragment of
            Nothing ->
              -- If we got a link that didn't include a fragment,
              -- it's from one of those (href="") attributes that
              -- we are using for new comments. There might be
              -- better options available.
              ( model, Cmd.none )

            Just _ ->
              ( model, Cmd.none )
        Browser.External href ->
          ( model, Nav.load href )

    GotPaste result ->
      case result of
        Ok paste ->
          ( Success
              { paste = paste
              , newComment = Nothing
              , commentAnchor = Nothing
              }
          , Cmd.none
          )

        Err err ->
          (Failure (Debug.toString err), Cmd.none)

    NewComment anchor ->
      case anchor of
        Line lno ->
          case model of
            Success root ->
              ( Success { root | commentAnchor = Just anchor }
              , Cmd.none
              )
            _ -> Debug.todo ("todo throw error " ++ Debug.toString model)

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
              (moveCommentToPaste root)
              --{ root |
              --    commentAnchor = Nothing,
              --    newComment = Nothing
              --}
          , postComment root
          )
        _ -> Debug.todo "todo throw error"

    Commented result ->
      case result of
        Ok _ ->
          case model of
            Success root ->
              -- Move comment to paste before reseting models
              ( Success root
                  --(moveCommentToPaste root)
                  --{ root |
                  --    commentAnchor = Nothing,
                  --    newComment = Nothing
                  --}
              , Cmd.none
              )
            _ -> Debug.todo "todo posted comment"
        Err _ -> (model, Cmd.none)

lineNumForAnchor : Anchor -> Int
lineNumForAnchor (Line n) = n

{-
lineNumForAnchor : Anchor -> Maybe Int
lineNumForAnchor anchor =
  case anchor of
    TopLevel  -> Nothing
    Line i    -> Just i
-}

onUrlChange : Url -> Msg
onUrlChange url = GotPaste (Err (BadUrl ("You cant change the url! " ++ (Url.toString url))))

-- onUrlRequest : UrlRequest -> Msg
-- onUrlRequest url =
--    GotPaste (Err (BadUrl ("You cant request a url! " ++ (Debug.toString url))))

onClickNoBubble : msg -> Html.Attribute msg
onClickNoBubble message =
    Html.Events.custom "click"
      (JD.succeed
          { message = message
          , stopPropagation = True
          , preventDefault = True
          }
      )

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
    f = viewLineWithComments root.commentAnchor root.newComment
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
      [ th []
          [ a
              --[ href ("#" ++ lno_str)
              [ href ""
              , onClickNoBubble (NewComment (Line lno))
              ]
              [ text lno_str ]
          ]
      , td [] [ text line ]
      ]

-- returning List coz too lazy to handle returning maybe
viewNewComment : Int -> Maybe Anchor -> Maybe Comment -> List (Html Msg)
viewNewComment lno maybeAnchor maybeComment =
  let
    anchorLine = M.map lineNumForAnchor maybeAnchor
  in
    if anchorLine == (Just lno) then
      [ viewCommentTextBox maybeComment ]
    else
      []

viewCommentTextBox : Maybe Comment -> Html Msg
viewCommentTextBox maybeComment =
  tr [ class "foo" ]
    [ td [ colspan 2 ]
        [ div []
            [ textarea
                [ class "text-white"
                , onInput SetCommentData
                , style "background" "0 0"
                , style "border" "0"
                ]
                (M.withDefault [] (M.map (\cmt -> [ text cmt.body ]) maybeComment))
            , br [] []
            , button [ class "btn", onClick MkComment ] [ text "Save" ]
            ]
        ]
    ]

viewLineComment : Comment -> Html Msg
viewLineComment comment =
  tr [ class "bg-info" ]
    [ td [ colspan 2 ]
        [ div []
            [ text comment.body ]
        ]
    ]

-- Http

getPasteFromServer : PasteId -> Cmd Msg
getPasteFromServer pasteId =
  Http.get
    { url = "http://localhost:8081/paste/" ++ pasteId
    , expect = Http.expectJson GotPaste pasteDecoder
    }

postComment : Root -> Cmd Msg
postComment root =
  let
    lno = M.map lineNumForAnchor root.commentAnchor
    qparam = M.map (\n -> "?line=" ++ (String.fromInt n)) lno
    pasteUrl =
      "http://localhost:8081/paste/" ++ root.paste.id ++ "/comment" ++ (M.withDefault "" qparam)
  in
    Http.post
      { url = pasteUrl
      , body =
          Http.stringBody
            "application/json"
            (JE.encode 0 (JE.string (M.withDefault {body=""} root.newComment).body))
      , expect = Http.expectWhatever Commented
      }

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

