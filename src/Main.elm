module Main exposing (..)

import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (src, class)
import Html.Events exposing (onClick)
import Http
import Dict exposing (Dict)


visibleCols : List String
visibleCols =
    [ "competitorname", "sugarpercent", "pricepercent", "winpercent" ]


filterableCols : List String
filterableCols =
    [ "fruity"
    , "caramel"
    , "peanutyalmondy"
    , "nougat"
    , "crispedricewafer"
    , "hard"
    , "bar"
    , "pluribus"
    ]



---- MODEL ----


type SortBy
    = Asc (Maybe String)
    | Desc (Maybe String)


type Filter
    = Include String
    | Exclude String
    | All String


type alias Model =
    { headers : List String
    , rows : List (Dict String String)
    , sortBy : SortBy
    , curFilters : List Filter
    }


initFilters : List Filter
initFilters =
    List.map All filterableCols


init : ( Model, Cmd Msg )
init =
    ( { headers = []
      , rows = []
      , sortBy = Asc Nothing
      , curFilters = initFilters
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
    | ToggleFilter Filter


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

        ToggleFilter filter ->
            let
                updatedFilters =
                    List.map (updateFilter filter) model.curFilters
            in
                ( { model | curFilters = updatedFilters }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateFilter : Filter -> Filter -> Filter
updateFilter updatingFilter curFilter =
    case updatingFilter of
        Include name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter

        Exclude name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter

        All name ->
            case matchFilter name curFilter of
                True ->
                    updatingFilter

                False ->
                    curFilter


matchFilter : String -> Filter -> Bool
matchFilter name filter =
    case filter of
        Include filterName ->
            name == filterName

        Exclude filterName ->
            name == filterName

        All filterName ->
            name == filterName



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


matchRow : Dict String String -> Filter -> Bool
matchRow row filter =
    case filter of
        Include key ->
            (Dict.get key row) == Just "1"

        Exclude key ->
            (Dict.get key row) == Just "0"

        All key ->
            case Dict.get key row of
                Nothing ->
                    False

                _ ->
                    True


applyFilters : List Filter -> Dict String String -> Bool
applyFilters filters row =
    List.all (matchRow row) filters


viewRows : Model -> Html Msg
viewRows ({ rows, sortBy, curFilters } as model) =
    let
        filteredRows =
            List.filter (applyFilters curFilters) rows

        sortedRows =
            List.sortWith (sortRows sortBy) filteredRows
    in
        div [] (List.map (viewRow model) sortedRows)


viewFilterOption : Filter -> List Filter -> Html Msg
viewFilterOption filter curFilters =
    let
        label =
            case filter of
                Include name ->
                    name

                Exclude name ->
                    "not " ++ name

                All name ->
                    "all"

        display =
            case List.member filter curFilters of
                True ->
                    "active-filter"

                False ->
                    ""
    in
        div [ onClick (ToggleFilter filter), class display ] [ text label ]


viewFilter : List Filter -> String -> Html Msg
viewFilter curFilters filterName =
    div [ class "cell" ]
        [ viewFilterOption (Include filterName) curFilters
        , viewFilterOption (Exclude filterName) curFilters
        , viewFilterOption (All filterName) curFilters
        ]


viewFilters : List Filter -> Html Msg
viewFilters curFilters =
    div [ class "row" ]
        (List.map (viewFilter curFilters) filterableCols)


viewRankings : Model -> Html Msg
viewRankings model =
    div []
        [ viewFilters model.curFilters
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
