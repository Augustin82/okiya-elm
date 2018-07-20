module Main exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Events exposing (..)
import Html exposing (Html)
import Process
import Task
import Time


---- MODEL ----


type Model
    = Loading
    | Playing Game


type alias Game =
    { board : Board
    , score : Score
    , player : Player
    , selected : Maybe TileId
    }


type alias Score =
    List ( Player, Int )


type alias Board =
    List Row


type alias Row =
    List Tile


type alias TileId =
    Int


type alias Tile =
    { tree : Tree
    , feature : Feature
    , player : Maybe Player
    , id : TileId
    }


type Tree
    = Pine
    | Cherry
    | Iris
    | Maple


trees : List Tree
trees =
    [ Pine, Cherry, Iris, Maple ]


treeToString : Tree -> String
treeToString tree =
    case tree of
        Pine ->
            "Pin"

        Cherry ->
            "Cerisier"

        Iris ->
            "Iris"

        Maple ->
            "Érable"


featureToString : Feature -> String
featureToString feature =
    case feature of
        Sun ->
            "Soleil"

        Rain ->
            "Pluie"

        Bird ->
            "Oiseau"

        Tanzaku ->
            "Tanzaku"


type Feature
    = Sun
    | Rain
    | Bird
    | Tanzaku


features : List Feature
features =
    [ Sun, Rain, Bird, Tanzaku ]


tiles : List Tile
tiles =
    trees
        |> List.indexedMap
            (\i t ->
                features
                    |> List.indexedMap (\j f -> Tile t f Nothing (i * 4 + j))
            )
        |> List.concat


stupidBoard : Board
stupidBoard =
    tiles
        |> List.foldr
            (\tile list ->
                case list of
                    [] ->
                        [ [ tile ] ]

                    h :: l ->
                        if List.length h < 4 then
                            (tile :: h) :: l
                        else
                            [ tile ] :: h :: l
            )
            []


type Player
    = Red
    | Blue


init : ( Model, Cmd Msg )
init =
    ( Loading, getSavedState )


cleanGame : Game
cleanGame =
    { board = stupidBoard, score = [ ( Red, 0 ), ( Blue, 0 ) ], player = Red, selected = Nothing }


getSavedState : Cmd Msg
getSavedState =
    Process.sleep (1 * Time.second)
        |> Task.andThen
            (\_ ->
                Task.succeed ReadLocalStorage
            )
        |> Task.perform identity



---- UPDATE ----


type Msg
    = NoOp
    | ReadLocalStorage
    | SelectTile Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReadLocalStorage ->
            ( Playing cleanGame, Cmd.none )

        SelectTile id ->
            ( model |> selectTile id |> nextPlayer, Cmd.none )

        _ ->
            ( model, Cmd.none )


nextPlayer : Model -> Model
nextPlayer model =
    case model of
        Playing game ->
            let
                newPlayer =
                    case game.player of
                        Red ->
                            Blue

                        Blue ->
                            Red
            in
            Playing { game | player = newPlayer }

        _ ->
            model


selectTile : Int -> Model -> Model
selectTile id model =
    case model of
        Playing game ->
            let
                board =
                    game.board

                player =
                    game.player

                newBoard =
                    List.map
                        (\r ->
                            List.map
                                (\t ->
                                    if t.id == id then
                                        { t | player = Just player }
                                    else
                                        t
                                )
                                r
                        )
                        board
            in
            Playing { game | board = newBoard, selected = Just id }

        _ ->
            model



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [] <|
        case model of
            Loading ->
                el [] <| text "Chargement en cours..."

            Playing game ->
                column []
                    [ viewBoard game ]


viewBoard : { a | board : Board, selected : Maybe TileId } -> Element Msg
viewBoard { board, selected } =
    let
        selectedTile =
            selected
                |> Maybe.andThen (getTileById board)

        viewSelectedTile =
            selectedTile
                |> Maybe.map viewBlankTile
                |> Maybe.withDefault (el [ centerX ] <| text "Choisissez une tuile sur les bords !")
    in
    board |> List.map (viewRow selectedTile) |> (::) viewSelectedTile |> column []


getTileById : Board -> TileId -> Maybe Tile
getTileById board id =
    board
        |> List.foldl
            (\r t1 ->
                case t1 of
                    Nothing ->
                        List.foldl
                            (\t t2 ->
                                case t2 of
                                    Nothing ->
                                        if t.id == id then
                                            Just t
                                        else
                                            Nothing

                                    _ ->
                                        t2
                            )
                            Nothing
                            r

                    _ ->
                        t1
            )
            Nothing


viewRow : Maybe Tile -> List Tile -> Element Msg
viewRow selectedTile rows =
    rows |> List.map (viewTile selectedTile) |> row []


viewTile : Maybe Tile -> Tile -> Element Msg
viewTile selectedTile ({ id, tree, feature, player } as tile) =
    el [ centerX, centerY, width <| px 150, height <| px 150 ] <|
        (player
            |> Maybe.map playerTile
            |> Maybe.withDefault (viewBlankTile tile |> clickWrapper selectedTile tile)
        )


clickWrapper : Maybe Tile -> Tile -> Element Msg -> Element Msg
clickWrapper selectedTile tile element =
    let
        isClickable =
            (tile.player == Nothing)
                && (case selectedTile of
                        Just st ->
                            tile.feature == st.feature || tile.tree == st.tree

                        Nothing ->
                            True
                   )

        clickAttrs =
            if isClickable then
                [ onClick <| SelectTile tile.id ]
            else
                []
    in
    el (clickAttrs ++ [ height fill, width fill ]) <|
        element


viewBlankTile : Tile -> Element Msg
viewBlankTile ({ id, tree, feature, player } as tile) =
    column
        [ height shrink
        , width shrink
        , centerY
        , centerX
        ]
        [ el [] <| text <| treeToString tree
        , el [] <| text <| featureToString feature
        ]


playerTile : Player -> Element msg
playerTile player =
    let
        color =
            case player of
                Red ->
                    Color.rgb 255 0 0

                Blue ->
                    Color.rgb 0 0 255
    in
    el [ width fill, height fill, Background.color color ] <| text " "



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
