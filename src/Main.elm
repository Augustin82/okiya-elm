port module Main exposing (..)

import Color
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes as HA
import Json.Decode as D
import Json.Decode.Pipeline as P
import Json.Encode as E
import Random
import Random.List


port loadGame : () -> Cmd msg


port saveGame : E.Value -> Cmd msg


port gameLoaded : (E.Value -> msg) -> Sub msg



---- MODEL ----


type Model
    = Loading
    | Playing Game
    | Draw Game
    | Victory Player Game


type alias Game =
    { board : Board
    , score : Score
    , player : Player
    , selected : Maybe TileId
    , showHelp : Bool
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


type Feature
    = Sun
    | Rain
    | Bird
    | Tanzaku


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
    ( Loading, loadGame () )


subscriptions : Model -> Sub Msg
subscriptions model =
    gameLoaded (decodeGame >> GameLoaded)


decodeGame : E.Value -> Maybe Game
decodeGame value =
    D.decodeValue gameDecoder value
        |> Result.toMaybe


gameDecoder : D.Decoder Game
gameDecoder =
    P.decode Game
        |> P.required "board" boardDecoder
        |> P.required "score" scoreDecoder
        |> P.required "player" playerDecoder
        |> P.required "selected" (D.nullable tileIdDecoder)
        |> P.hardcoded False


scoreDecoder : D.Decoder Score
scoreDecoder =
    D.keyValuePairs D.int
        |> D.map
            (List.map
                (Tuple.mapFirst
                    (\s ->
                        if s == "Red" then
                            Red
                        else
                            Blue
                    )
                )
            )


boardDecoder : D.Decoder Board
boardDecoder =
    D.list rowDecoder


rowDecoder : D.Decoder Row
rowDecoder =
    D.list tileDecoder


tileDecoder : D.Decoder Tile
tileDecoder =
    P.decode Tile
        |> P.required "tree" treeDecoder
        |> P.required "feature" featureDecoder
        |> P.required "player" (D.nullable playerDecoder)
        |> P.required "id" tileIdDecoder
        |> P.required "selectable" D.bool
        |> P.required "coords" coordsDecoder


gameEncoder : Game -> E.Value
gameEncoder { board, score, player, selected } =
    E.object
        [ ( "board", boardEncoder board )
        , ( "score", scoreEncoder score )
        , ( "player", playerEncoder player )
        , ( "selected", selected |> Maybe.map E.int |> Maybe.withDefault E.null )
        ]


boardEncoder : Board -> E.Value
boardEncoder =
    List.map rowEncoder >> E.list


rowEncoder : List Tile -> E.Value
rowEncoder =
    List.map tileEncoder >> E.list


tileEncoder : Tile -> E.Value
tileEncoder { tree, feature, player, id, selectable, coords } =
    E.object
        [ ( "tree", treeEncoder tree )
        , ( "feature", featureEncoder feature )
        , ( "player", player |> Maybe.map playerEncoder |> Maybe.withDefault E.null )
        , ( "id", E.int id )
        , ( "selectable", E.bool selectable )
        , ( "coords", coordsEncoder coords )
        ]


scoreEncoder : Score -> E.Value
scoreEncoder score =
    score
        |> List.map (Tuple.mapFirst toString >> Tuple.mapSecond E.int)
        |> E.object


treeEncoder : Tree -> E.Value
treeEncoder =
    toString >> E.string


featureEncoder : Feature -> E.Value
featureEncoder =
    toString >> E.string


playerEncoder : Player -> E.Value
playerEncoder =
    toString >> E.string


coordsEncoder : Coords -> E.Value
coordsEncoder { x, y } =
    E.object [ ( "x", E.int x ), ( "y", E.int y ) ]


treeDecoder : D.Decoder Tree
treeDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Pine" ->
                        D.succeed Pine

                    "Iris" ->
                        D.succeed Iris

                    "Cherry" ->
                        D.succeed Cherry

                    "Maple" ->
                        D.succeed Maple

                    _ ->
                        D.fail "Unknown tree"
            )


featureDecoder : D.Decoder Feature
featureDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Bird" ->
                        D.succeed Bird

                    "Sun" ->
                        D.succeed Sun

                    "Rain" ->
                        D.succeed Rain

                    "Tanzaku" ->
                        D.succeed Tanzaku

                    _ ->
                        D.fail "Unknown feature"
            )


coordsDecoder : D.Decoder Coords
coordsDecoder =
    P.decode Coords
        |> P.required "x" D.int
        |> P.required "y" D.int


playerDecoder : D.Decoder Player
playerDecoder =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "Red" ->
                        D.succeed Red

                    "Blue" ->
                        D.succeed Blue

                    _ ->
                        D.fail <| "Unknown player: " ++ s
            )


tileIdDecoder : D.Decoder TileId
tileIdDecoder =
    D.int


startScore : Score
startScore =
    [ ( Red, 0 )
    , ( Blue, 0 )
    ]


cleanGame : Game
cleanGame =
    { board = [], score = startScore, player = Red, selected = Nothing, showHelp = False }


generateRandomBoard : Cmd Msg
generateRandomBoard =
    Random.generate GeneratedRandomBoard <| Random.List.shuffle tiles



---- UPDATE ----


type Msg
    = NoOp
    | Reset
    | GeneratedRandomBoard (List Tile)
    | SelectTile Int
    | GameLoaded (Maybe Game)
    | ToggleHelp


addBoard : Model -> List Tile -> Game
addBoard model tiles =
    let
        newBoard =
            makeBoardFromTiles tiles
    in
    (\g -> { g | board = newBoard }) <|
        case model of
            Playing game ->
                game

            Victory _ game ->
                game

            Draw game ->
                game

            Loading ->
                cleanGame


toggleHelp : Model -> Model
toggleHelp model =
    case model of
        Playing game ->
            Playing { game | showHelp = not game.showHelp }

        Victory player game ->
            Victory player { game | showHelp = not game.showHelp }

        Draw game ->
            Draw { game | showHelp = not game.showHelp }

        Loading ->
            Loading


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleHelp ->
            ( toggleHelp model, Cmd.none )

        GameLoaded maybeGame ->
            case maybeGame of
                Nothing ->
                    ( model, generateRandomBoard )

                Just game ->
                    ( Playing game, Cmd.none )

        GeneratedRandomBoard tiles ->
            let
                game =
                    addBoard model tiles
            in
            ( Playing game, game |> gameEncoder |> saveGame )

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

                        cmd =
                            case newModel of
                                Playing _ ->
                                    newGame |> gameEncoder |> saveGame

                                _ ->
                                    E.null |> saveGame
                    in
                    ( newModel, cmd )

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
        |> List.filter (.player >> (==) Nothing)
        |> (==) []


noPlayLeft : Game -> Bool
noPlayLeft newGame =
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
        Draw game
    else if noPlayLeft game then
        winGame player game
    else if hasFullLine player game then
        winGame player game
    else if hasSquare player game then
        winGame player game
    else
        Playing game


winGame : Player -> Game -> Model
winGame winner game =
    let
        newScore =
            game.score
                |> List.map
                    (\(( player, score ) as t) ->
                        if player == winner then
                            ( winner, score + 1 )
                        else
                            t
                    )

        loser =
            if winner == Red then
                Blue
            else
                Red

        newGame =
            { game | score = newScore, player = loser }
    in
    Victory winner newGame


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
    layout [] <|
        el [ padding 20, width <| px 600, centerX ] <|
            column [ spacing 20 ] <|
                case model of
                    Loading ->
                        [ el [] <| text "Chargement en cours..." ]

                    Playing game ->
                        [ viewInstructions game.showHelp, viewScore game, viewBoard game, resetButton ]

                    Draw game ->
                        [ viewInstructions game.showHelp, viewScore game, viewBoard game, viewDrawMessage, resetButton ]

                    Victory player game ->
                        [ viewInstructions game.showHelp, viewScore game, viewBoard game, viewVictoryMessage player, resetButton ]


viewScore : Game -> Element Msg
viewScore { score } =
    let
        getScore player =
            score
                |> List.filter (Tuple.first >> (==) player)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0
                |> toString
    in
    column []
        [ el [ centerX ] <| text "Score"
        , row [ spacing 20, width shrink, centerX ]
            [ row [ spacing 10 ] [ playerTurnIndicator Red Red, getScore Red |> text ]
            , text " - "
            , row [ spacing 10 ] [ getScore Blue |> text, playerTurnIndicator Blue Blue ]
            ]
        ]


viewInstructions : Bool -> Element Msg
viewInstructions showHelp =
    el
        [ below <|
            if showHelp then
                el [ paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ] <|
                    textColumn
                        [ spacing 10
                        , height <| px 400
                        , Background.color <| Color.rgb 0 0 0
                        , Font.color <| Color.rgb 255 255 255
                        , padding 10
                        , Font.justify
                        , alignRight
                        , width shrink
                        , height shrink
                        ]
                        [ paragraph []
                            [ text "À tour de rôle, chaque joueur remplace une tuile par un de ses pions."
                            ]
                        , paragraph []
                            [ text "On ne peut choisir une tuile que si elle a un point commun (arbre ou symbole) avec la dernière tuile choisie. Au début de la partie, on ne peut choisir qu'une tuile sur les bords."
                            ]
                        , textColumn []
                            [ paragraph []
                                [ text "Un joueur gagne quand :"
                                ]
                            , paragraph []
                                [ text "- il forme une ligne (horizontale, verticale, diagonale) de quatre pions, ou"
                                ]
                            , paragraph []
                                [ text "- il forme un carré de quatre pions, ou"
                                ]
                            , paragraph []
                                [ text "- son adversaire n'a plus d'endroit où jouer."
                                ]
                            ]
                        , paragraph []
                            [ text "Si tous les joueurs placent leurs tuiles et qu'il n'y a pas de vainqueur, c'est un match nul."
                            ]
                        ]
            else
                none
        , onMouseEnter ToggleHelp
        , onMouseLeave ToggleHelp
        , onClick ToggleHelp
        , Font.underline
        , pointer
        , mouseOver <| [ Font.color <| Color.rgb 0 0 255 ]
        , width fill
        ]
    <|
        el [ alignRight ] <|
            text "Comment jouer ?"


resetButton : Element Msg
resetButton =
    el
        [ centerY
        , centerX
        , width shrink
        ]
    <|
        Input.button
            [ Background.color <| Color.rgb 150 150 150
            , Font.color <| Color.rgb 255 255 255
            , padding 10
            , width <| px 150
            ]
            { onPress = Just Reset
            , label = text "Recommencer"
            }


viewVictoryMessage : Player -> Element Msg
viewVictoryMessage player =
    el
        [ centerX
        , Background.color <| getPlayerColor player
        , Font.color <| Color.rgb 255 255 255
        , Font.bold
        , Font.size 30
        , width fill
        , padding 20
        ]
    <|
        text <|
            "Victoire du joueur "
                ++ (if player == Red then
                        "rouge !"
                    else
                        "bleu !"
                   )


viewDrawMessage : Element Msg
viewDrawMessage =
    el [ centerX ] <| text "Match nul !"


viewBoard : { a | board : Board, player : Player, selected : Maybe TileId } -> Element Msg
viewBoard { board, player, selected } =
    let
        selectedTile =
            selected
                |> Maybe.andThen (getTileById board)
    in
    column [ width shrink, centerX, spacing 20 ]
        [ viewGameInfoHeader player selectedTile
        , board |> List.map (viewRow selectedTile) |> column [ width shrink, Border.width 1 ]
        ]


viewGameInfoHeader : Player -> Maybe Tile -> Element Msg
viewGameInfoHeader currentPlayer selectedTile =
    row [ height <| px 100 ]
        [ playerTurnIndicator Red currentPlayer
        , selectedTile
            |> Maybe.map tileIndicator
            |> Maybe.withDefault (el [ centerX ] <| text "Choisissez une tuile sur les bords")
        , playerTurnIndicator Blue currentPlayer
        ]


tileIndicator : Tile -> Element Msg
tileIndicator tile =
    column
        [ centerX
        , width shrink
        , height shrink
        ]
        [ text "Choisissez une tuile avec :"
        , paragraph [ centerX, width shrink ]
            [ tile.tree |> treeToString |> text
            , text " ou "
            , tile.feature |> featureToString |> text
            ]
        ]


playerTurnIndicator : Player -> Player -> Element Msg
playerTurnIndicator player currentPlayer =
    let
        color =
            if player == currentPlayer then
                getPlayerColor player
            else
                Color.rgb 255 255 255
    in
    el [ centerY, height <| px 30, width <| px 30, Border.color color, Background.color color, Border.width 1, Border.rounded 15 ] <| text " "


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
    el [ centerX, centerY, width <| px 110, height <| px 110, Border.width 1 ] <|
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
                [ pointer, onClick <| SelectTile tile.id ]
            else
                [ pointer, Background.color <| Color.rgba 0 0 0 0.3 ]
    in
    el (clickAttrs ++ [ height fill, width fill, Border.color <| Color.rgb 0 0 0 ]) <|
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
    el
        [ htmlAttribute <| HA.style [ ( "cursor", "not-allowed" ) ]
        , centerY
        , centerX
        , width <| px 90
        , height <| px 90
        , Border.rounded 45
        , Background.color (getPlayerColor player)
        ]
    <|
        text " "


getPlayerColor : Player -> Color.Color
getPlayerColor player =
    case player of
        Red ->
            Color.rgb 255 0 0

        Blue ->
            Color.rgb 125 125 200



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
