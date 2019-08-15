module Game exposing (Game, Player, Team(..), decodeGame, maybeMakeGame, playersOnTeam, teamOf)

import Dict
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



------ NETWORK ------


maybeMakeGame : String -> (Result Http.Error Game -> a) -> Cmd a
maybeMakeGame id msg =
    Http.post
        { url = "http://localhost:8080/new-game"
        , body = Http.jsonBody (Enc.object [ ( "game_id", Enc.string id ) ])
        , expect = Http.expectJson msg decodeGame
        }


playersOnTeam : Game -> Team -> Int
playersOnTeam g team =
    g.players
        |> Dict.values
        |> List.filter (\x -> x.team == team)
        |> List.length


teamOf : Game -> String -> Team
teamOf game playerId =
    Maybe.withDefault NoTeam (Maybe.map (\p -> p.team) (Dict.get playerId game.players))


decodeGame : Dec.Decoder Game
decodeGame =
    Dec.map8 Game
        (Dec.field "state" (Dec.field "seed" Dec.int))
        (Dec.field "state" (Dec.field "round" Dec.int))
        (Dec.field "words" (Dec.list Dec.string))
        (Dec.field "state" (Dec.field "exposed_two" (Dec.list Dec.bool)))
        (Dec.field "state" (Dec.field "exposed_two" (Dec.list Dec.bool)))
        (Dec.field "state" (Dec.field "players" (Dec.dict decodePlayer)))
        (Dec.field "one_layout" (Dec.list Dec.string))
        (Dec.field "two_layout" (Dec.list Dec.string))


decodePlayer : Dec.Decoder Player
decodePlayer =
    Dec.map2 Player
        (Dec.field "team" decodeTeam)
        (Dec.field "lastSeen" Dec.string)


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
