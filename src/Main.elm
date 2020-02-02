module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder)
import Debug
import Maybe
import List exposing (map)
import Array exposing (Array, map)

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
  { units: Array Unit
  , showBattle: Bool
  }

type alias Unit =
  { health : Int
  , strength : Int
  }


type Msg
  = NextRound
  | ShowBattle Cell
  | GotBoard (Result Http.Error BoardState)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NextRound ->
      (Loading, getBoardState)

    ShowBattle cell ->
      (model, Cmd.none)

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
    , div [class "content"]
    [ viewGif model
    , button [class "btn", onClick NextRound ] [text "Next"]
    ]
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

type alias Team = String

viewBoardState : BoardState -> Html Msg
viewBoardState state =
  div [class "board"]
  [ viewBoard state.red "red"
  , viewBoard state.blue "blue"
  ]

viewBoard : Board -> Team -> Html Msg
viewBoard board rvb =
  div [class "board-half", class rvb] (List.map viewRow board)

viewRow : List (Maybe Cell) -> Html Msg
viewRow row =
  div [class "row"] (List.map viewCell row)

viewCell : Maybe Cell -> Html Msg
viewCell maybeCell =
  case maybeCell of
    Just cell ->
      let
        noOfUnits = Array.length cell.units
      in
      if noOfUnits == 1 then
        let
            unit = case Array.get 0 cell.units of
              Just u -> u
              Nothing -> {strength= 0, health= 0}
        in
        div [class "cell"] [viewUnit unit]
      else if noOfUnits > 1 then
        div [class "cell"] [viewBattle cell]
      else
        div [class "cell"] [text "?"]
    Nothing ->
      div [class "cell", class "cell-empty"] []

viewUnit : Unit -> Html Msg
viewUnit unit =
  div [class "unit"]
  [ div [class "strength"] [text (String.fromInt unit.strength)]
  , div [class "health"] [text (String.fromInt unit.health)]
  ]

viewBattle : Cell -> Html Msg
viewBattle cell =
  let
      battleCell = if cell.showBattle then
                    viewBattleDetails cell
                  else
                    text (String.fromInt (Array.length cell.units))
  in
  div [class "battle", onClick (ShowBattle cell)] [battleCell]

viewBattleDetails : Cell -> Html Msg
viewBattleDetails cell =
  let
      unitToCell = \u -> Just {units = Array.fromList [u], showBattle = False}
  in
  div [class "battleDetails"]
    (Array.toList (Array.map  (\u -> viewCell (unitToCell u)) cell.units))

getBoardState : Cmd Msg
getBoardState =
  Http.get
    { url = "http://localhost:8080/next"
    , expect = Http.expectJson GotBoard boardStateDecoder
    }

boardStateDecoder : Decoder BoardState
boardStateDecoder =
  Json.Decode.map2 BoardState
  (Json.Decode.field "red" boardDecoder)
  (Json.Decode.field "blue" boardDecoder)

boardDecoder : Decoder (List (List (Maybe Cell)))
boardDecoder =
  Json.Decode.list (Json.Decode.list (Json.Decode.nullable cellDecoder))

cellDecoder : Decoder Cell
cellDecoder =
  Json.Decode.map2 Cell
  (Json.Decode.field "units" (Json.Decode.array unitDecoder))
  (Json.Decode.succeed True)

unitDecoder : Decoder Unit
unitDecoder =
  Json.Decode.map2 Unit
  (Json.Decode.field "health" Json.Decode.int)
  (Json.Decode.field "strength" Json.Decode.int)
  -- (Json.Decode.field "rvb" Json.Decode.int)

