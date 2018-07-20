module Main exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Html exposing (Html)
import Random
import Random.List


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
    , selectable : Bool
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
                    |> List.indexedMap (\j f -> Tile t f Nothing (i * 4 + j) False)
            )
        |> List.concat


makeBoardFromTiles : List Tile -> Board
makeBoardFromTiles tiles =
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
        |> List.indexedMap
            (\i row ->
                row
                    |> List.indexedMap
                        (\j t ->
                            if i == 0 || i == 3 || j == 0 || j == 3 then
                                { t | selectable = True }
                            else
                                t
                        )
            )


type Player
    = Red
    | Blue


init : ( Model, Cmd Msg )
init =
    ( Loading, generateRandomBoard )


cleanGame : List Tile -> Game
cleanGame randomTiles =
    { board = makeBoardFromTiles randomTiles, score = [ ( Red, 0 ), ( Blue, 0 ) ], player = Red, selected = Nothing }


generateRandomBoard : Cmd Msg
generateRandomBoard =
    Random.generate GeneratedRandomBoard <| Random.List.shuffle tiles



---- UPDATE ----


type Msg
    = NoOp
    | GeneratedRandomBoard (List Tile)
    | SelectTile Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedRandomBoard randomBoard ->
            ( Playing <| cleanGame randomBoard, Cmd.none )

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

                selectedTile =
                    getTileById board id

                newBoard =
                    List.map
                        (\r ->
                            List.map
                                (\t ->
                                    if t.id == id then
                                        { t | player = Just player, selectable = False }
                                    else
                                        { t | selectable = selectedTile |> Maybe.map (isValidSelection t) |> Maybe.withDefault t.selectable }
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
            |> Maybe.withDefault (tile |> viewBlankTile |> clickWrapper tile)
        )


isValidSelection : Tile -> Tile -> Bool
isValidSelection tile st =
    tile.feature == st.feature || tile.tree == st.tree


clickWrapper : Tile -> Element Msg -> Element Msg
clickWrapper tile element =
    let
        clickAttrs =
            if tile.selectable then
                [ onClick <| SelectTile tile.id ]
            else
                [ Background.color <| Color.rgba 0 0 0 0.3 ]
    in
    el (clickAttrs ++ [ height fill, width fill, Border.color <| Color.rgb 0 0 0, Border.width 1 ]) <|
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
