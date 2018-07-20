module Main exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Random
import Random.List


---- MODEL ----


type Model
    = Loading
    | Playing Game
    | Victory Player Game


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
    , coords : Coords
    }


type alias Coords =
    { x : Int
    , y : Int
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
                    |> List.indexedMap (\j f -> Tile t f Nothing (i * 4 + j) False (Coords 0 0))
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
                                { t | selectable = True, coords = Coords i j }
                            else
                                { t | coords = Coords i j }
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
    | Reset
    | GeneratedRandomBoard (List Tile)
    | SelectTile Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GeneratedRandomBoard randomBoard ->
            ( Playing <| cleanGame randomBoard, Cmd.none )

        SelectTile id ->
            case model of
                Playing game ->
                    let
                        currentPlayer =
                            game.player

                        newGame =
                            game |> selectTile id |> nextPlayer

                        newModel =
                            evaluateVictory currentPlayer newGame
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Reset ->
            ( model, generateRandomBoard )

        NoOp ->
            ( model, Cmd.none )


noTilesLeft : Game -> Bool
noTilesLeft newGame =
    newGame
        |> .board
        |> List.concat
        |> List.filter .selectable
        |> (==) []


hasFullLine : Player -> Game -> Bool
hasFullLine player newGame =
    let
        selectedTiles =
            newGame
                |> .board
                |> List.concat
                |> List.filter (\tile -> tile.player == Just player)

        hasStraightLine axis =
            [ 0, 1, 2, 3 ]
                |> List.foldl
                    (\pos acc ->
                        acc
                            || (selectedTiles
                                    |> List.filter (.coords >> axis >> (==) pos)
                                    |> List.length
                                    |> (==) 4
                               )
                    )
                    False

        hasTopLeftDiagonal _ =
            selectedTiles
                |> List.filter (\t -> t.coords == Coords 0 0 || t.coords == Coords 1 1 || t.coords == Coords 2 2 || t.coords == Coords 3 3)
                |> List.length
                |> (==) 4

        hasBottomLeftDiagonal _ =
            selectedTiles
                |> List.filter (\t -> t.coords == Coords 0 3 || t.coords == Coords 1 2 || t.coords == Coords 2 1 || t.coords == Coords 3 0)
                |> List.length
                |> (==) 4
    in
    hasStraightLine .y || hasStraightLine .x || hasTopLeftDiagonal () || hasBottomLeftDiagonal ()


hasSquare : Player -> Game -> Bool
hasSquare player game =
    let
        selectedTiles =
            game
                |> .board
                |> List.concat
                |> List.filter (\tile -> tile.player == Just player)
    in
    [ 0, 1, 2 ]
        |> List.foldl
            (\startX acc ->
                acc
                    || ([ 0, 1, 2 ]
                            |> List.foldl
                                (\startY acc ->
                                    acc
                                        || (selectedTiles
                                                |> List.filter (\tile -> (tile.coords.x == startX || tile.coords.x == startX + 1) && (tile.coords.y == startY || tile.coords.y == startY + 1))
                                                |> List.length
                                                |> (==) 4
                                           )
                                )
                                False
                       )
            )
            False


evaluateVictory : Player -> Game -> Model
evaluateVictory player game =
    if noTilesLeft game then
        Victory player game
    else if hasFullLine player game then
        Victory player game
    else if hasSquare player game then
        Victory player game
    else
        Playing game


nextPlayer : Game -> Game
nextPlayer game =
    let
        newPlayer =
            case game.player of
                Red ->
                    Blue

                Blue ->
                    Red
    in
    { game | player = newPlayer }


selectTile : Int -> Game -> Game
selectTile id game =
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
                                { t
                                    | selectable =
                                        t.player
                                            |> Maybe.map (always False)
                                            |> Maybe.withDefault
                                                (selectedTile
                                                    |> Maybe.map (isValidSelection t)
                                                    |> Maybe.withDefault t.selectable
                                                )
                                }
                        )
                        r
                )
                board
    in
    { game | board = newBoard, selected = Just id }



---- VIEW ----


view : Model -> Html Msg
view model =
    layout [ padding 20 ] <|
        case model of
            Loading ->
                el [] <| text "Chargement en cours..."

            Playing game ->
                column [ spacing 20 ]
                    [ viewBoard game, resetButton ]

            Victory player game ->
                column [ spacing 20 ]
                    [ viewBoard game, viewVictoryMessage player, resetButton ]


resetButton : Element Msg
resetButton =
    el [ centerX, centerY ] <|
        Input.button
            [ Background.color <| Color.rgb 150 150 150
            , Font.color <| Color.rgb 255 255 255
            , padding 10
            ]
            { onPress = Just Reset
            , label = text "Recommencer"
            }


viewVictoryMessage : Player -> Element Msg
viewVictoryMessage player =
    el [ centerX ] <|
        text <|
            "Victoire du joueur "
                ++ (if player == Red then
                        "rouge !"
                    else
                        "bleu !"
                   )


viewBoard : { a | board : Board, selected : Maybe TileId } -> Element Msg
viewBoard { board, selected } =
    let
        selectedTile =
            selected
                |> Maybe.andThen (getTileById board)

        viewSelectedTile =
            selectedTile
                |> Maybe.map (\t -> row [ centerX, width shrink, spacing 20 ] [ text "Dernière tuile :", viewBlankTile t ])
                |> Maybe.withDefault (el [ centerX ] <| text "Choisissez une tuile sur les bords !")
    in
    column [ spacing 20 ]
        [ viewSelectedTile
        , board |> List.map (viewRow selectedTile) |> column []
        ]


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
