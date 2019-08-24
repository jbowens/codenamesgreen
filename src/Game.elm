module Game exposing (Model, Msg(..), init, update, viewBoard, viewEventLog, viewKeycard, viewStatus)

import Api exposing (Event, Update)
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
import Player exposing (Player)
import Side exposing (Side)
import Task
import User exposing (User)


init : Api.GameState -> User -> Api.Client -> ( Model, Cmd Msg )
init state user client =
    let
        model =
            List.foldl applyEvent
                { id = state.id
                , seed = state.seed
                , players = Dict.empty
                , events = []
                , cells =
                    List.map3 (\w l1 l2 -> ( w, ( False, l1 ), ( False, l2 ) ))
                        state.words
                        state.oneLayout
                        state.twoLayout
                        |> List.indexedMap (\i ( w, ( e1, l1 ), ( e2, l2 ) ) -> Cell i w ( e1, l1 ) ( e2, l2 ))
                        |> Array.fromList
                , player = { id = user.playerId, name = user.name, side = Nothing }
                , turn = Nothing
                , tokensConsumed = 0
                , client = client
                }
                state.events

        player =
            { id = user.playerId, name = user.name, side = Dict.get user.playerId model.players }

        modelWithPlayer =
            { model | player = player }
    in
    ( modelWithPlayer, Cmd.batch [ longPollEvents modelWithPlayer, jumpToBottom "events" ] )



------ MODEL ------


type alias Model =
    { id : String
    , seed : String
    , players : Dict.Dict String Side
    , events : List Api.Event
    , cells : Array Cell
    , player : Player
    , turn : Maybe Side
    , tokensConsumed : Int
    , client : Api.Client
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
            if exposedBlack <| Array.toList <| g.cells then
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
    | LongPoll String String (Result Http.Error Api.Update)
    | GameUpdate (Result Http.Error Api.Update)
    | WordPicked Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LongPoll id seed result ->
            case ( id == model.id && seed == model.seed, result ) of
                ( False, _ ) ->
                    -- We might get the result of a long poll from a previous
                    -- game we were playing, in which case we just want to
                    -- ignore it.
                    ( model, Cmd.none )

                ( True, Err e ) ->
                    -- Even if the long poll request failed for some reason,
                    -- we want to trigger a new request anyways. The failure
                    -- could be short-lived.
                    ( model, longPollEvents model )

                ( True, Ok up ) ->
                    let
                        ( m, cmd ) =
                            applyUpdate model up
                    in
                    ( m, Cmd.batch [ cmd, longPollEvents m ] )

        GameUpdate (Ok up) ->
            applyUpdate model up

        GameUpdate (Err err) ->
            -- TODO: flash an error message?
            ( model, Cmd.none )

        WordPicked cell ->
            case model.player.side of
                Nothing ->
                    ( model, Cmd.none )

                Just side ->
                    ( model
                    , if not (Cell.isExposed (Side.opposite side) cell) then
                        Api.submitGuess
                            { gameId = model.id
                            , player = model.player
                            , index = cell.index
                            , lastEventId = lastEvent model
                            , toMsg = GameUpdate
                            , client = model.client
                            }

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
        , events = e :: model.events
        , turn =
            if Cell.sideColor (Side.opposite side) cell == Color.Tan then
                Just (Side.opposite side)

            else
                Just side
        , tokensConsumed =
            case ( model.turn == Just side, Cell.sideColor (Side.opposite side) cell ) of
                ( False, Color.Tan ) ->
                    -- If it's not your turn, we need to consume one
                    -- token to end the previous side's turn at guessing.
                    -- If the currently guessing team hit a tan, they're
                    -- done guessing and we need to consume a second.
                    model.tokensConsumed + 2

                ( _, Color.Tan ) ->
                    model.tokensConsumed + 1

                ( False, _ ) ->
                    model.tokensConsumed + 1

                _ ->
                    model.tokensConsumed
    }


longPollEvents : Model -> Cmd Msg
longPollEvents m =
    Api.longPollEvents
        { gameId = m.id
        , player = m.player
        , lastEventId = lastEvent m
        , toMsg = LongPoll m.id m.seed
        , client = m.client
        }



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
                [ text e.name
                , text " has joined side "
                , text (e.side |> Maybe.map Side.toString |> Maybe.withDefault "")
                , text "."
                ]
            ]

        "player_left" ->
            [ div [] [ text e.name, text " has left the game." ] ]

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
            [ div [] [ text e.name, text ": ", text e.message ] ]

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
