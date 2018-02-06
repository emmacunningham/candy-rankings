module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)
import Http
import Dict exposing (Dict)


visibleCols : List String
visibleCols =
    [ "competitorname", "sugarpercent", "pricepercent", "winpercent" ]



---- MODEL ----


type SortBy
    = Asc (Maybe String)
    | Desc (Maybe String)


type alias Model =
    { headers : List String
    , rows : List (Dict String String)
    , sortBy : SortBy
    }


init : ( Model, Cmd Msg )
init =
    ( { headers = []
      , rows = []
      , sortBy = Asc Nothing
      }
    , getRawCsv
    )



---- COMMANDS ----


getRawCsv : Cmd Msg
getRawCsv =
    Http.getString "https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv"
        |> Http.send ReceiveCsv



---- UPDATE ----


type Msg
    = NoOp
    | ReceiveCsv (Result Http.Error String)
    | ToggleSortBy String


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

                sortBy =
                    Asc (List.head headers)
            in
                ( { model
                    | headers = headers
                    , rows = dictRows
                    , sortBy = sortBy
                  }
                , Cmd.none
                )

        ToggleSortBy key ->
            case model.sortBy of
                Asc _ ->
                    ( { model | sortBy = Desc (Just key) }, Cmd.none )

                Desc _ ->
                    ( { model | sortBy = Asc (Just key) }, Cmd.none )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


viewHeader : SortBy -> String -> Html Msg
viewHeader sortBy header =
    let
        sortClass =
            case sortBy of
                Asc (Just key) ->
                    case key == header of
                        True ->
                            "asc cell"

                        False ->
                            "cell"

                Desc (Just key) ->
                    case key == header of
                        True ->
                            "desc cell"

                        False ->
                            "cell"

                _ ->
                    "cell"

        classes =
            case List.member header visibleCols of
                True ->
                    sortClass

                False ->
                    sortClass ++ " hidden"
    in
        div [ class classes, onClick (ToggleSortBy header) ] [ text header ]


viewHeaders : List String -> SortBy -> Html Msg
viewHeaders headers sortBy =
    div [ class "row" ] (List.map (viewHeader sortBy) headers)


viewCell : String -> Html Msg
viewCell cell =
    div [ class "cell" ] [ text cell ]


viewRow : Model -> Dict String String -> Html Msg
viewRow model row =
    let
        visibleKeys =
            List.filter ((flip List.member) visibleCols) model.headers

        rowText =
            List.map (\x -> Maybe.withDefault "" (Dict.get x row)) visibleKeys
    in
        div [ class "row" ] (List.map viewCell rowText)


sortRows : SortBy -> Dict String String -> Dict String String -> Order
sortRows sortBy a b =
    case sortBy of
        Asc (Just key) ->
            let
                compA =
                    Dict.get key a |> Maybe.withDefault ""

                compB =
                    Dict.get key b |> Maybe.withDefault ""
            in
                compare compA compB

        Desc (Just key) ->
            let
                compA =
                    Dict.get key a |> Maybe.withDefault ""

                compB =
                    Dict.get key b |> Maybe.withDefault ""
            in
                case compare compA compB of
                    LT ->
                        GT

                    GT ->
                        LT

                    EQ ->
                        EQ

        _ ->
            EQ


viewRows : Model -> Html Msg
viewRows ({ rows, sortBy } as model) =
    let
        sortedRows =
            List.sortWith (sortRows sortBy) rows
    in
        div [] (List.map (viewRow model) sortedRows)


viewFilters : Html Msg
viewFilters =
    div [ class "row" ]
        []


viewRankings : Model -> Html Msg
viewRankings model =
    div []
        [ viewFilters
        , viewHeaders model.headers model.sortBy
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
