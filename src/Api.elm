module Api exposing (Client, Event, GameState, Update, init, longPollEvents, maybeMakeGame, submitGuess)

import Color exposing (Color)
import Http
import Json.Decode as D
import Json.Encode as E
import Player exposing (Player)
import Side exposing (Side)
import Url


init : Url.Url -> Client
init url =
    let
        baseUrl =
            case url.host of
                "localhost" ->
                    { url | port_ = Just 8080, path = "", query = Nothing, fragment = Nothing }

                "www.codenamesgreen.com" ->
                    -- TODO: Avoid hardcoding any specific hostnames.
                    { url | host = "api.codenamesgreen.com", path = "", query = Nothing, fragment = Nothing }

                _ ->
                    { url | host = "api." ++ url.host, path = "", query = Nothing, fragment = Nothing }
    in
    { baseUrl = baseUrl }


type alias Client =
    { baseUrl : Url.Url
    }


type alias GameState =
    { seed : String
    , words : List String
    , events : List Event
    , oneLayout : List Color
    , twoLayout : List Color
    }


type alias Event =
    { number : Int
    , typ : String
    , playerId : String
    , name : String
    , side : Maybe Side
    , index : Int
    , message : String
    }


type alias Update =
    { seed : String
    , events : List Event
    }


endpointUrl : Url.Url -> String -> String
endpointUrl baseUrl path =
    { baseUrl | path = path }
        |> Url.toString


submitGuess : String -> Player -> Int -> Int -> (Result Http.Error Update -> msg) -> Client -> Cmd msg
submitGuess gameId player cellIndex lastEventId toMsg client =
    Http.post
        { url = endpointUrl client.baseUrl "/guess"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string gameId )
                    , ( "index", E.int cellIndex )
                    , ( "player_id", E.string player.id )
                    , ( "name", E.string player.name )
                    , ( "team", Side.encodeMaybe player.side )
                    , ( "last_event", E.int lastEventId )
                    ]
                )
        , expect = Http.expectJson toMsg decodeUpdate
        }


longPollEvents : String -> Player -> Int -> (Result Http.Error Update -> msg) -> String -> Client -> Cmd msg
longPollEvents gameId player lastEventId toMsg tracker client =
    Http.request
        { method = "POST"
        , headers = []
        , url = endpointUrl client.baseUrl "/events"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string gameId )
                    , ( "player_id", E.string player.id )
                    , ( "name", E.string player.name )
                    , ( "team", Side.encodeMaybe player.side )
                    , ( "last_event", E.int lastEventId )
                    ]
                )
        , expect = Http.expectJson toMsg decodeUpdate
        , timeout = Just 45000
        , tracker = Just tracker
        }


maybeMakeGame : String -> Maybe String -> (Result Http.Error GameState -> msg) -> Client -> Cmd msg
maybeMakeGame id prevSeed toMsg client =
    Http.post
        { url = endpointUrl client.baseUrl "/new-game"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string id )
                    , ( "prev_seed"
                      , case prevSeed of
                            Nothing ->
                                E.null

                            Just seed ->
                                E.string seed
                      )
                    ]
                )
        , expect = Http.expectJson toMsg decodeGameState
        }


decodeGameState : D.Decoder GameState
decodeGameState =
    D.map5 GameState
        (D.field "state" (D.field "seed" D.string))
        (D.field "words" (D.list D.string))
        (D.field "state" (D.field "events" (D.list decodeEvent)))
        (D.field "one_layout" (D.list Color.decode))
        (D.field "two_layout" (D.list Color.decode))


decodeUpdate : D.Decoder Update
decodeUpdate =
    D.map2 Update
        (D.field "seed" D.string)
        (D.field "events" (D.list decodeEvent))


decodeEvent : D.Decoder Event
decodeEvent =
    D.map7 Event
        (D.field "number" D.int)
        (D.field "type" D.string)
        (D.field "player_id" D.string)
        (D.field "name" D.string)
        (D.field "team" Side.decodeMaybe)
        (D.field "index" D.int)
        (D.field "message" D.string)
