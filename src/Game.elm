module Game exposing (GameData, Model, Msg(..), Player, init, maybeMakeGame, update, viewBoard, viewEventLog, viewKeycard, viewStatus)

import Array exposing (Array)
import Browser.Dom as Dom
import Cell exposing (Cell)
import Color exposing (Color)
import Dict
import Html exposing (Html, div, h3, i, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy3)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Side exposing (Side)
import Task


init : String -> GameData -> String -> String -> ( Model, Cmd Msg )
init id data playerId apiUrl =
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
                , turn = Nothing
                , tokensConsumed = 0
                , apiUrl = apiUrl
                }
                data.events

        player =
            { id = playerId, side = Dict.get playerId model.players }

        modelWithPlayer =
            { model | player = player }
    in
    ( modelWithPlayer, Cmd.batch [ longPollEvents modelWithPlayer, jumpToBottom "events" ] )



------ MODEL ------


type alias Model =
    { id : String
    , seed : String
    , players : Dict.Dict String Side
    , events : List Event
    , cells : Array Cell
    , player : Player
    , turn : Maybe Side
    , tokensConsumed : Int
    , apiUrl : String
    }


type alias GameData =
    { seed : String
    , words : List String
    , events : List Event
    , oneLayout : List Color
    , twoLayout : List Color
    }


type alias Update =
    { seed : String
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


type Status
    = Start
    | InProgress Side Int Int
    | Lost Int
    | Won Int


lastEvent : Model -> Int
lastEvent m =
    m.events
        |> List.head
        |> Maybe.map (\x -> x.number)
        |> Maybe.withDefault 0


status : Model -> Status
status g =
    let
        greens =
            remainingGreen g.cells
    in
    case g.turn of
        Nothing ->
            Start

        Just turn ->
            if exposedBlack (Array.toList g.cells) then
                Lost greens

            else if greens == 0 then
                Won g.tokensConsumed

            else
                InProgress turn greens g.tokensConsumed


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
    = NoOp
    | LongPoll String (Result Http.Error Update)
    | GameUpdate (Result Http.Error Update)
    | WordPicked Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LongPoll seed result ->
            case ( seed == model.seed, result ) of
                ( False, _ ) ->
                    ( model, Cmd.none )

                ( _, Err e ) ->
                    ( model, Cmd.none )

                ( True, Ok up ) ->
                    let
                        ( updatedModel, cmd ) =
                            applyUpdate model up
                    in
                    ( updatedModel, Cmd.batch [ cmd, longPollEvents updatedModel ] )

        GameUpdate (Ok up) ->
            applyUpdate model up

        GameUpdate (Err err) ->
            ( model, Cmd.none )

        WordPicked cell ->
            case model.player.side of
                Nothing ->
                    ( model, Cmd.none )

                Just side ->
                    ( model
                    , if not (Cell.isExposed (Side.opposite side) cell) then
                        submitGuess model.apiUrl model.id model.player cell (lastEvent model)

                      else
                        Cmd.none
                    )


applyUpdate : Model -> Update -> ( Model, Cmd Msg )
applyUpdate model up =
    if up.seed /= model.seed then
        ( model, Cmd.none )

    else
        let
            newModel =
                List.foldl applyEvent model up.events
        in
        ( newModel
        , if lastEvent newModel > lastEvent model then
            jumpToBottom "events"

          else
            Cmd.none
        )


applyEvent : Event -> Model -> Model
applyEvent e model =
    if e.number <= lastEvent model then
        model

    else
        case e.typ of
            "join_side" ->
                { model | players = Dict.update e.playerId (\_ -> e.side) model.players, events = e :: model.events }

            "player_left" ->
                { model | players = Dict.update e.playerId (\_ -> Nothing) model.players, events = e :: model.events }

            "guess" ->
                case ( Array.get e.index model.cells, e.side ) of
                    ( Just cell, Just side ) ->
                        applyGuess e cell side model

                    _ ->
                        { model | events = e :: model.events }

            _ ->
                { model | events = e :: model.events }


applyGuess : Event -> Cell -> Side -> Model -> Model
applyGuess e cell side model =
    { model
        | cells = Array.set e.index (Cell.tapped side cell) model.cells
        , turn =
            if Cell.sideColor (Side.opposite side) cell == Color.Tan then
                Just (Side.opposite side)

            else
                Just side
        , tokensConsumed =
            if model.turn == Just (Side.opposite side) || Cell.sideColor (Side.opposite side) cell == Color.Tan then
                model.tokensConsumed + 1

            else
                model.tokensConsumed
        , events = e :: model.events
    }



------ NETWORK ------


maybeMakeGame : String -> String -> Maybe String -> (Result Http.Error GameData -> a) -> Cmd a
maybeMakeGame apiUrl id prevSeed msg =
    Http.post
        { url = apiUrl ++ "/new-game"
        , body =
            Http.jsonBody
                (Enc.object
                    [ ( "game_id", Enc.string id )
                    , ( "prev_seed"
                      , case prevSeed of
                            Nothing ->
                                Enc.null

                            Just seed ->
                                Enc.string seed
                      )
                    ]
                )
        , expect = Http.expectJson msg decodeGameData
        }


submitGuess : String -> String -> Player -> Cell -> Int -> Cmd Msg
submitGuess apiUrl gameId player cell lastEventId =
    Http.post
        { url = apiUrl ++ "/guess"
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
        , url = model.apiUrl ++ "/events"
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
        (Dec.field "state" (Dec.field "seed" Dec.string))
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "state" (Dec.field "events" (Dec.list decodeEvent)))
        (Dec.field "one_layout" (Dec.list Color.decode))
        (Dec.field "two_layout" (Dec.list Color.decode))


decodeUpdate : Dec.Decoder Update
decodeUpdate =
    Dec.map2 Update
        (Dec.field "seed" Dec.string)
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


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (always NoOp)


viewStatus : Model -> Html a
viewStatus model =
    case status model of
        Start ->
            div [ Attr.id "status", Attr.class "in-progress" ]
                [ text "Either side may give the first clue!" ]

        Lost _ ->
            div [ Attr.id "status", Attr.class "lost" ]
                [ text "You lost :(" ]

        Won _ ->
            div [ Attr.id "status", Attr.class "won" ]
                [ text "You won!" ]

        InProgress turn greens tokensConsumed ->
            div [ Attr.id "status", Attr.class "in-progress" ]
                [ text (String.fromInt greens)
                , span [ Attr.class "green-icon" ] []
                , text " | "
                , text (String.fromInt tokensConsumed)
                , text " "
                , i [ Attr.class "icon ion-ios-time" ] []
                ]


viewBoard : Model -> Html Msg
viewBoard model =
    Keyed.node "div"
        [ Attr.id "board"
        , Attr.classList
            [ ( "no-team", model.player.side == Nothing )
            ]
        ]
        (model.cells
            |> Array.toList
            |> List.map (\c -> ( c.word, lazy3 Cell.view model.player.side WordPicked c ))
        )


viewEventLog : Model -> Html Msg
viewEventLog model =
    div [ Attr.id "event-log" ]
        [ div [ Attr.id "events" ]
            (model.events
                |> List.reverse
                |> List.concatMap (viewEvent model)
            )
        ]


viewEvent : Model -> Event -> List (Html Msg)
viewEvent model e =
    case e.typ of
        "join_side" ->
            [ div []
                [ text "A new player has joined side "
                , text (e.side |> Maybe.map Side.toString |> Maybe.withDefault "")
                , text "."
                ]
            ]

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
