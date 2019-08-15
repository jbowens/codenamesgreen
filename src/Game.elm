module Game exposing (Cell, Game, Player, Team(..), cells, guess, maybeMakeGame, playersOnTeam, teamOf, viewBoard, viewKeycard)

import Dict
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import Json.Encode as Enc



------ MODEL ------


type Team
    = NoTeam
    | A
    | B


type alias Game =
    { seed : Int
    , round : Int
    , words : List String
    , exposedOne : List Bool
    , exposedTwo : List Bool
    , players : Dict.Dict String Player
    , oneLayout : List String
    , twoLayout : List String
    }


type alias Player =
    { team : Team
    , lastSeen : String
    }


type alias Cell =
    { index : Int
    , word : String
    , a : ( Bool, String )
    , b : ( Bool, String )
    }


cells : Game -> List Cell
cells g =
    List.indexedMap
        (\i ( w, ( e1, l1 ), ( e2, l2 ) ) -> Cell i w ( e1, l1 ) ( e2, l2 ))
        (List.map5
            (\w e1 e2 l1 l2 -> ( w, ( e1, l1 ), ( e2, l2 ) ))
            g.words
            g.exposedOne
            g.exposedTwo
            g.oneLayout
            g.twoLayout
        )


playersOnTeam : Game -> Team -> Int
playersOnTeam g team =
    g.players
        |> Dict.values
        |> List.filter (\x -> x.team == team)
        |> List.length


teamOf : Game -> String -> Team
teamOf game playerId =
    Maybe.withDefault NoTeam (Maybe.map (\p -> p.team) (Dict.get playerId game.players))



------ NETWORK ------


maybeMakeGame : String -> (Result Http.Error Game -> a) -> Cmd a
maybeMakeGame id msg =
    Http.post
        { url = "http://localhost:8080/new-game"
        , body = Http.jsonBody (Enc.object [ ( "game_id", Enc.string id ) ])
        , expect = Http.expectJson msg decodeGame
        }


guess : String -> String -> Cell -> Team -> (Result Http.Error Game -> a) -> Cmd a
guess gameId playerId cell team msg =
    Http.post
        { url = "http://localhost:8080/guess"
        , body =
            Http.jsonBody
                (Enc.object
                    [ ( "game_id", Enc.string gameId )
                    , ( "index", Enc.int cell.index )
                    , ( "player_id", Enc.string playerId )
                    , ( "team", encodeTeam team )
                    ]
                )
        , expect = Http.expectJson msg decodeGame
        }


decodeGame : Dec.Decoder Game
decodeGame =
    Dec.map8 Game
        (Dec.field "state" (Dec.field "seed" Dec.int))
        (Dec.field "state" (Dec.field "round" Dec.int))
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "state" (Dec.field "exposed_one" (Dec.list Dec.bool)))
        (Dec.field "state" (Dec.field "exposed_two" (Dec.list Dec.bool)))
        (Dec.field "state" (Dec.field "players" (Dec.dict decodePlayer)))
        (Dec.field "one_layout" (Dec.list Dec.string))
        (Dec.field "two_layout" (Dec.list Dec.string))


decodePlayer : Dec.Decoder Player
decodePlayer =
    Dec.map2 Player
        (Dec.field "team" decodeTeam)
        (Dec.field "last_seen" Dec.string)


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


viewBoard : Game -> Team -> (Cell -> a) -> Html a
viewBoard g t msg =
    div [ Attr.id "board" ]
        (List.map
            (\c -> viewCell c t msg)
            (cells g)
        )


viewCell : Cell -> Team -> (Cell -> a) -> Html a
viewCell cell team msg =
    let
        exposedGreen =
            cell.a == ( True, "g" ) || cell.b == ( True, "g" )

        exposedBlack =
            cell.a == ( True, "b" ) || cell.b == ( True, "b" )

        pickable =
            case team of
                A ->
                    not (Tuple.first cell.b) && not exposedGreen && not exposedBlack

                B ->
                    not (Tuple.first cell.a) && not exposedGreen && not exposedBlack

                NoTeam ->
                    False
    in
    div
        [ Attr.classList
            [ ( "cell", True )
            , ( "green", exposedGreen )
            , ( "black", exposedBlack )
            , ( "pickable", pickable )
            ]
        , onClick (msg cell)
        ]
        [ text cell.word ]


viewKeycard : Game -> Team -> Html a
viewKeycard game team =
    let
        mySide =
            if team == A then
                game.oneLayout

            else
                game.twoLayout
    in
    div [ Attr.id "key-card" ]
        (List.map
            (\c ->
                div
                    [ Attr.class "cell"
                    , Attr.class
                        (case c of
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
            mySide
        )
