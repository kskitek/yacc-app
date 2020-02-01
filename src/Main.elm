module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, list, int, map2, nullable)
import Debug
import Maybe
import List exposing (map)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type Model
  = Failure
  | Loading
  | Success BoardState

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getBoardState)

type alias BoardState =
  { red: Board
  , blue: Board
  }

type alias Board = List (List (Maybe Cell))

type alias Cell =
  { health : Int
  , strength : Int
  }

type Msg
  = MorePlease
  | GotBoard (Result Http.Error BoardState)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (Loading, getBoardState)

    GotBoard result ->
      case result of
        Ok board ->
          (Success board, Cmd.none)

        Err err ->
          let
              _ = Debug.log (logErr err) ""
          in
          (Failure, Cmd.none)

logErr : Http.Error -> String
logErr err =
  case err of
        Http.Timeout ->
            "Timeout exceeded"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
                String.fromInt code

        Http.BadBody text ->
            "Unexpected response from api: " ++ text

        Http.BadUrl url ->
            "Malformed url: " ++ url

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


view : Model -> Html Msg
view model =
  div []
    [ Html.node "link" [rel "stylesheet", href "style.css" ] []
    , div [class "content"] [viewGif model]
    ]

viewGif : Model -> Html Msg
viewGif model =
  case model of
    Failure ->
      text "Failure!!"

    Loading ->
      text "Loading..."

    Success board ->
      viewBoardState board

type alias RVB = String

viewBoardState : BoardState -> Html Msg
viewBoardState state =
  div [class "board"]
  [ viewBoard state.red "red"
  , viewBoard (List.reverse state.blue) "blue"
  ]

viewBoard : Board -> RVB -> Html Msg
viewBoard board rvb =
  div [class "board-half", class rvb] (List.map viewRow board)

viewRow : List (Maybe Cell) -> Html Msg
viewRow row =
  div [class "row"] (List.map viewCell row)

viewCell : Maybe Cell -> Html Msg
viewCell cell =
  case cell of
    Just c ->
      div [class "cell"] [
        div [class "strength"] [text (String.fromInt c.strength)]
        , div [class "health"] [text (String.fromInt c.health)]
        ]
    Nothing ->
      div [class "cell", class "cell-empty"] []

getBoardState : Cmd Msg
getBoardState =
  Http.get
    { url = "http://localhost:8080/start"
    , expect = Http.expectJson GotBoard boardStateDecoder
    }

boardStateDecoder : Decoder BoardState
boardStateDecoder =
  map2 BoardState
  (field "red" boardDecoder)
  (field "blue" boardDecoder)

boardDecoder : Decoder (List (List (Maybe Cell)))
boardDecoder =
  Json.Decode.list (Json.Decode.list (nullable cellDecoder))

cellDecoder : Decoder Cell
cellDecoder =
  map2 Cell
  (field "health" int)
  (field "strength" int)

