module Game exposing (GameData, Model, Msg(..), Player, init, maybeMakeGame, teamOf, update, viewBoard, viewEventLog, viewKeycard, viewStatus)

import Array exposing (Array)
import Cell exposing (Cell)
import Color exposing (Color)
import Dict
import Html exposing (Html, div, h3, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Team exposing (Team)


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
                , player = { id = playerId, team = Team.None }
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
    , team : Team
    , index : Int
    }


type alias Player =
    { id : String
    , team : Team
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


teamOf : Model -> String -> Team
teamOf model playerId =
    model.players
        |> Dict.get playerId
        |> Maybe.withDefault Team.None



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
            ( model
            , if model.player.team /= Team.None && not (Cell.isExposed (Team.opposite model.player.team) cell) then
                submitGuess model.id model.player cell (lastEvent model)

              else
                Cmd.none
            )


applyEvent : Event -> Model -> Model
applyEvent e model =
    case e.typ of
        "new_player" ->
            { model | players = Dict.update e.playerId (\_ -> Just e.team) model.players, events = e :: model.events }

        "player_left" ->
            { model | players = Dict.update e.playerId (\_ -> Nothing) model.players, events = e :: model.events }

        "set_team" ->
            { model | players = Dict.update e.playerId (\_ -> Just e.team) model.players, events = e :: model.events }

        "guess" ->
            case Array.get e.index model.cells of
                Just cell ->
                    { model
                        | cells = Array.set e.index (tapped model.player.team cell) model.cells
                        , events = e :: model.events
                    }

                Nothing ->
                    { model | events = e :: model.events }

        _ ->
            { model | events = e :: model.events }


tapped : Team -> Cell -> Cell
tapped team cell =
    case team of
        Team.B ->
            { cell | a = ( True, Tuple.second cell.a ) }

        Team.A ->
            { cell | b = ( True, Tuple.second cell.b ) }

        _ ->
            cell



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
                    , ( "team", Team.encode player.team )
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
        (Dec.field "one_layout" (Dec.list Color.decode))
        (Dec.field "two_layout" (Dec.list Color.decode))


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
        (Dec.field "team" Team.decode)
        (Dec.field "index" Dec.int)



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
            cell.a == ( True, Color.Green ) || cell.b == ( True, Color.Green )

        black =
            cell.a == ( True, Color.Black ) || cell.b == ( True, Color.Black )

        pickable =
            case team of
                Team.A ->
                    not (Tuple.first cell.b) && not green && not black

                Team.B ->
                    not (Tuple.first cell.a) && not green && not black

                Team.None ->
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


viewEventLog : Model -> Html Msg
viewEventLog model =
    div [ Attr.id "event-log" ]
        [ h3 [] [ text "Activity log" ]
        , div [ Attr.class "events" ]
            (model.events
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
                |> Maybe.map
                    (\c ->
                        [ div []
                            [ text "Side "
                            , text (Team.toString e.team)
                            , text " tapped "
                            , span [] [ text c.word ]
                            ]
                        ]
                    )
                |> Maybe.withDefault []

        _ ->
            []


viewKeycard : Model -> Team -> Html a
viewKeycard model team =
    let
        mySide =
            if team == Team.A then
                \c -> Tuple.second c.a

            else
                \c -> Tuple.second c.b
    in
    div [ Attr.id "key-card" ]
        (model.cells
            |> Array.toList
            |> List.map
                (\c ->
                    div
                        [ Attr.class "cell"
                        , Attr.class (Color.toString (mySide c))
                        ]
                        []
                )
        )
