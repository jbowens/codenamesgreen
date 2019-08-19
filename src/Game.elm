module Game exposing (GameData, Model, Msg(..), Player, init, maybeMakeGame, update, viewBoard, viewEventLog, viewKeycard, viewStatus)

import Array exposing (Array)
import Cell exposing (Cell)
import Color exposing (Color)
import Dict
import Html exposing (Html, div, h3, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Side exposing (Side)


init : String -> GameData -> String -> ( Model, Cmd Msg )
init id data playerId =
    let
        model =
            List.foldl applyEvent
                { id = id
                , seed = data.seed
                , players = Dict.empty
                , events = []
                , cells =
                    List.map3 (\w l1 l2 -> ( w, ( False, l1 ), ( False, l2 ) ))
                        data.words
                        data.oneLayout
                        data.twoLayout
                        |> List.indexedMap (\i ( w, ( e1, l1 ), ( e2, l2 ) ) -> Cell i w ( e1, l1 ) ( e2, l2 ))
                        |> Array.fromList
                , player = { id = playerId, side = Nothing }
                }
                data.events

        player =
            { id = playerId, side = Dict.get playerId model.players }

        modelWithPlayer =
            { model | player = player }
    in
    ( modelWithPlayer, longPollEvents modelWithPlayer )



------ MODEL ------


type alias Model =
    { id : String
    , seed : Int
    , players : Dict.Dict String Side
    , events : List Event
    , cells : Array Cell
    , player : Player
    }


type alias GameData =
    { seed : Int
    , words : List String
    , events : List Event
    , oneLayout : List Color
    , twoLayout : List Color
    }


type alias Update =
    { seed : Int
    , events : List Event
    }


type alias Event =
    { number : Int
    , typ : String
    , playerId : String
    , side : Maybe Side
    , index : Int
    , message : String
    }


type alias Player =
    { id : String
    , side : Maybe Side
    }


lastEvent : Model -> Int
lastEvent m =
    m.events
        |> List.head
        |> Maybe.map (\x -> x.number)
        |> Maybe.withDefault 0


remainingGreen : Array Cell -> Int
remainingGreen cells =
    15
        - (cells
            |> Array.map Cell.display
            |> Array.filter (\x -> x == Cell.ExposedGreen)
            |> Array.length
          )


exposedBlack : List Cell -> Bool
exposedBlack cells =
    cells
        |> List.map Cell.display
        |> List.any (\x -> x == Cell.ExposedBlack)



------ UPDATE ------


type Msg
    = LongPoll Int (Result Http.Error Update)
    | GameUpdate (Result Http.Error Update)
    | WordPicked Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LongPoll seed result ->
            let
                updatedModel =
                    case result of
                        Ok up ->
                            applyUpdate model up

                        Err err ->
                            model
            in
            ( updatedModel
            , if seed == updatedModel.seed then
                longPollEvents updatedModel

              else
                Cmd.none
            )

        GameUpdate (Ok up) ->
            ( applyUpdate model up, Cmd.none )

        GameUpdate (Err err) ->
            ( model, Cmd.none )

        WordPicked cell ->
            case model.player.side of
                Nothing ->
                    ( model, Cmd.none )

                Just side ->
                    ( model
                    , if not (Cell.isExposed (Side.opposite side) cell) then
                        submitGuess model.id model.player cell (lastEvent model)

                      else
                        Cmd.none
                    )


applyUpdate : Model -> Update -> Model
applyUpdate model up =
    if up.seed == model.seed then
        List.foldl applyEvent model up.events

    else
        -- TODO: propagate the fact that the game is over
        model


applyEvent : Event -> Model -> Model
applyEvent e model =
    case e.typ of
        "new_player" ->
            { model | players = Dict.update e.playerId (\_ -> e.side) model.players, events = e :: model.events }

        "player_left" ->
            { model | players = Dict.update e.playerId (\_ -> Nothing) model.players, events = e :: model.events }

        "set_team" ->
            { model | players = Dict.update e.playerId (\_ -> e.side) model.players, events = e :: model.events }

        "guess" ->
            case ( Array.get e.index model.cells, e.side ) of
                ( Just cell, Just side ) ->
                    { model
                        | cells = Array.set e.index (Cell.tapped side cell) model.cells
                        , events = e :: model.events
                    }

                _ ->
                    { model | events = e :: model.events }

        _ ->
            { model | events = e :: model.events }



------ NETWORK ------


maybeMakeGame : String -> (Result Http.Error GameData -> a) -> Cmd a
maybeMakeGame id msg =
    Http.post
        { url = "http://localhost:8080/new-game"
        , body = Http.jsonBody (Enc.object [ ( "game_id", Enc.string id ) ])
        , expect = Http.expectJson msg decodeGameData
        }


submitGuess : String -> Player -> Cell -> Int -> Cmd Msg
submitGuess gameId player cell lastEventId =
    Http.post
        { url = "http://localhost:8080/guess"
        , body =
            Http.jsonBody
                (Enc.object
                    [ ( "game_id", Enc.string gameId )
                    , ( "index", Enc.int cell.index )
                    , ( "player_id", Enc.string player.id )
                    , ( "team", Side.encodeMaybe player.side )
                    , ( "last_event", Enc.int lastEventId )
                    ]
                )
        , expect = Http.expectJson GameUpdate decodeUpdate
        }


longPollEvents : Model -> Cmd Msg
longPollEvents model =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://localhost:8080/events" -- TODO: fix
        , body =
            Http.jsonBody
                (Enc.object
                    [ ( "game_id", Enc.string model.id )
                    , ( "player_id", Enc.string model.player.id )
                    , ( "team", Side.encodeMaybe model.player.side )
                    , ( "last_event", Enc.int (lastEvent model) )
                    ]
                )
        , expect = Http.expectJson (LongPoll model.seed) decodeUpdate
        , timeout = Just 45000
        , tracker = Nothing
        }


decodeGameData : Dec.Decoder GameData
decodeGameData =
    Dec.map5 GameData
        (Dec.field "state" (Dec.field "seed" Dec.int))
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "state" (Dec.field "events" (Dec.list decodeEvent)))
        (Dec.field "one_layout" (Dec.list Color.decode))
        (Dec.field "two_layout" (Dec.list Color.decode))


decodeUpdate : Dec.Decoder Update
decodeUpdate =
    Dec.map2 Update
        (Dec.field "seed" Dec.int)
        (Dec.field "events" (Dec.list decodeEvent))


decodeEvent : Dec.Decoder Event
decodeEvent =
    Dec.map6 Event
        (Dec.field "number" Dec.int)
        (Dec.field "type" Dec.string)
        (Dec.field "player_id" Dec.string)
        (Dec.field "team" Side.decodeMaybe)
        (Dec.field "index" Dec.int)
        (Dec.field "message" Dec.string)



------ VIEW ------


viewStatus : Model -> Html a
viewStatus g =
    let
        greens =
            remainingGreen g.cells
    in
    if exposedBlack (Array.toList g.cells) then
        -- handle time tokens
        div [ Attr.id "status", Attr.class "lost" ]
            [ text "You lost :(" ]

    else if greens == 0 then
        div [ Attr.id "status", Attr.class "won" ]
            [ text "You won!" ]

    else
        div [ Attr.id "status", Attr.class "in-progress" ]
            [ text (String.fromInt greens), text " agents remaining" ]


viewBoard : Model -> Html Msg
viewBoard model =
    Keyed.node "div"
        [ Attr.id "board" ]
        (model.cells
            |> Array.toList
            |> List.map (\c -> ( c.word, lazy3 Cell.view model.player.side WordPicked c ))
        )


viewEventLog : Model -> Html Msg
viewEventLog model =
    div [ Attr.id "event-log" ]
        [ div [ Attr.class "events" ]
            (model.events
                |> List.reverse
                |> List.concatMap (viewEvent model)
            )
        ]


viewEvent : Model -> Event -> List (Html Msg)
viewEvent model e =
    case e.typ of
        "new_player" ->
            [ div [] [ text "A new player has joined the game." ] ]

        "player_left" ->
            [ div [] [ text "A player has left the game." ] ]

        "guess" ->
            Array.get e.index model.cells
                |> Maybe.map2
                    (\s c ->
                        [ div []
                            [ text "Side "
                            , text (Side.toString s)
                            , text " tapped "
                            , span [ Attr.class "chat-color", Attr.class (Color.toString (Cell.sideColor (Side.opposite s) c)) ] [ text c.word ]
                            , text "."
                            ]
                        ]
                    )
                    e.side
                |> Maybe.withDefault []

        "chat" ->
            [ div [] [ text e.message ] ]

        _ ->
            []


viewKeycard : Model -> Side -> Html a
viewKeycard model side =
    div [ Attr.id "key-card" ]
        (model.cells
            |> Array.toList
            |> List.map (Cell.sideColor side)
            |> List.map
                (\c ->
                    div
                        [ Attr.class "cell"
                        , Attr.class (Color.toString c)
                        ]
                        []
                )
        )
