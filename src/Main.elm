module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)
import Json.Decode as Decode exposing (int, string, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Http
import Dict exposing (Dict)


---- MODEL ----


type alias Model =
    { headers : List String, rows : List (Dict String String) }


init : ( Model, Cmd Msg )
init =
    ( { headers = [], rows = [] }, getRawCsv )



---- COMMANDS ----


getRawCsv : Cmd Msg
getRawCsv =
    Http.getString "https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv"
        |> Http.send ReceiveCsv



---- UPDATE ----


type Msg
    = NoOp
    | ReceiveCsv (Result Http.Error String)


rowToDict : List String -> List String -> Dict String String
rowToDict headers row =
    List.map2 (,) headers row
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveCsv (Ok data) ->
            let
                stringsList =
                    String.split "\n" data

                headers =
                    List.head stringsList
                        |> Maybe.withDefault ""
                        |> String.split ","

                rows =
                    List.tail stringsList
                        |> Maybe.withDefault []
                        |> List.map (String.split ",")

                dictRows =
                    List.map (rowToDict headers) rows
            in
                ( { model | headers = headers, rows = dictRows }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


viewHeaders : List String -> Html Msg
viewHeaders headers =
    div [ class "row" ] (List.map viewCell headers)


viewCell : String -> Html Msg
viewCell cell =
    div [ class "cell" ] [ text cell ]


viewRow : Model -> Dict String String -> Html Msg
viewRow model row =
    let
        rowText =
            List.map (\x -> Maybe.withDefault "" (Dict.get x row)) model.headers
    in
        div [ class "row" ] (List.map viewCell rowText)


viewRows : Model -> Html Msg
viewRows ({ rows } as model) =
    div [] (List.map (viewRow model) rows)


viewRankings : Model -> Html Msg
viewRankings model =
    div []
        [ viewHeaders model.headers
        , viewRows model
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Rankings" ]
        , viewRankings model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
