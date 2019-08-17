module Game exposing (Cell, GameData, Model, Msg(..), Player, Team(..), init, maybeMakeGame, teamOf, update, viewBoard, viewKeycard, viewStatus)

import Array exposing (Array)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import Json.Encode as Enc


init : String -> GameData -> String -> Model
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
                , player = { id = playerId, team = NoTeam }
                }
                data.events

        player =
            { id = playerId, team = teamOf model playerId }
    in
    { model | player = player }



------ MODEL ------


type alias Model =
    { id : String
    , seed : Int
    , players : Dict.Dict String Team
    , events : List Event
    , cells : Array Cell
    , player : Player
    }


type alias GameData =
    { seed : Int
    , words : List String
    , events : List Event
    , oneLayout : List String
    , twoLayout : List String
    }


type alias Update =
    { seed : Int
    , events : List Event
    }


type alias Event =
    { number : Int
    , typ : String
    , playerId : String
    , team : Team
    , index : Int
    }


type Team
    = NoTeam
    | A
    | B


type alias Player =
    { id : String
    , team : Team
    }


type alias Cell =
    { index : Int
    , word : String
    , a : ( Bool, String )
    , b : ( Bool, String )
    }


lastEvent : Model -> Int
lastEvent m =
    case m.events of
        [] ->
            0

        x :: _ ->
            x.number


remainingGreen : Array Cell -> Int
remainingGreen c =
    Array.foldl
        (\x a ->
            case ( x.a, x.b ) of
                ( ( False, "g" ), ( True, "g" ) ) ->
                    a

                ( ( True, "g" ), ( False, "g" ) ) ->
                    a

                ( ( False, "g" ), _ ) ->
                    a + 1

                ( _, ( False, "g" ) ) ->
                    a + 1

                _ ->
                    a
        )
        0
        c


exposedBlack : List Cell -> Bool
exposedBlack c =
    List.any
        (\x ->
            case ( x.a, x.b ) of
                ( ( True, "b" ), _ ) ->
                    True

                ( _, ( True, "b" ) ) ->
                    True

                _ ->
                    False
        )
        c


teamOf : Model -> String -> Team
teamOf model playerId =
    model.players
        |> Dict.get playerId
        |> Maybe.withDefault NoTeam



------ UPDATE ------


type Msg
    = GameUpdate (Result Http.Error Update)
    | WordPicked Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameUpdate (Ok up) ->
            if up.seed == model.seed then
                ( List.foldl applyEvent model up.events, Cmd.none )

            else
                -- TODO: propagate the fact that the game is over
                ( model, Cmd.none )

        GameUpdate (Err err) ->
            ( model, Cmd.none )

        WordPicked cell ->
            ( model, submitGuess model.id model.player cell (lastEvent model) )


applyEvent : Event -> Model -> Model
applyEvent e model =
    case e.typ of
        "new_player" ->
            { model | players = Dict.update e.playerId (\_ -> Just e.team) model.players, events = e :: model.events }

        "player_left" ->
            { model | players = Dict.update e.playerId (\_ -> Nothing) model.players, events = e :: model.events }

        "set_team" ->
            { model | players = Dict.update e.playerId (\_ -> Just e.team) model.players, events = e :: model.events }

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
                    , ( "team", encodeTeam player.team )
                    , ( "last_event", Enc.int lastEventId )
                    ]
                )
        , expect = Http.expectJson GameUpdate decodeUpdate
        }


decodeGameData : Dec.Decoder GameData
decodeGameData =
    Dec.map5 GameData
        (Dec.field "state" (Dec.field "seed" Dec.int))
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "state" (Dec.field "events" (Dec.list decodeEvent)))
        (Dec.field "one_layout" (Dec.list Dec.string))
        (Dec.field "two_layout" (Dec.list Dec.string))


decodeUpdate : Dec.Decoder Update
decodeUpdate =
    Dec.map2 Update
        (Dec.field "seed" Dec.int)
        (Dec.field "events" (Dec.list decodeEvent))


decodeEvent : Dec.Decoder Event
decodeEvent =
    Dec.map5 Event
        (Dec.field "number" Dec.int)
        (Dec.field "type" Dec.string)
        (Dec.field "player_id" Dec.string)
        (Dec.field "team" decodeTeam)
        (Dec.field "index" Dec.int)


decodeTeam : Dec.Decoder Team
decodeTeam =
    Dec.int
        |> Dec.andThen
            (\i ->
                case i of
                    1 ->
                        Dec.succeed A

                    2 ->
                        Dec.succeed B

                    _ ->
                        Dec.succeed NoTeam
            )


encodeTeam : Team -> Enc.Value
encodeTeam t =
    Enc.int
        (case t of
            A ->
                1

            B ->
                2

            NoTeam ->
                0
        )



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
    div [ Attr.id "board" ]
        (List.map
            (\c -> viewCell c model.player.team)
            (Array.toList model.cells)
        )


viewCell : Cell -> Team -> Html Msg
viewCell cell team =
    let
        green =
            cell.a == ( True, "g" ) || cell.b == ( True, "g" )

        black =
            cell.a == ( True, "b" ) || cell.b == ( True, "b" )

        pickable =
            case team of
                A ->
                    not (Tuple.first cell.b) && not green && not black

                B ->
                    not (Tuple.first cell.a) && not green && not black

                NoTeam ->
                    False
    in
    div
        [ Attr.classList
            [ ( "cell", True )
            , ( "green", green )
            , ( "black", black )
            , ( "pickable", pickable )
            ]
        , onClick (WordPicked cell)
        ]
        [ text cell.word ]


viewKeycard : Model -> Team -> Html a
viewKeycard model team =
    let
        mySide =
            if team == A then
                \c -> Tuple.second c.a

            else
                \c -> Tuple.second c.b
    in
    div [ Attr.id "key-card" ]
        (List.map
            (\c ->
                div
                    [ Attr.class "cell"
                    , Attr.class
                        (case mySide c of
                            "g" ->
                                "green"

                            "b" ->
                                "black"

                            "t" ->
                                "tan"

                            _ ->
                                "unknown"
                        )
                    ]
                    []
            )
            (Array.toList model.cells)
        )
