module Api exposing (Client, Event, GameState, Update, init, longPollEvents, maybeMakeGame, ping, submitGuess)

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


submitGuess :
    { gameId : String
    , player : Player
    , index : Int
    , lastEventId : Int
    , toMsg : Result Http.Error Update -> msg
    , client : Client
    }
    -> Cmd msg
submitGuess r =
    Http.post
        { url = endpointUrl r.client.baseUrl "/guess"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string r.gameId )
                    , ( "index", E.int r.index )
                    , ( "player_id", E.string r.player.id )
                    , ( "name", E.string r.player.name )
                    , ( "team", Side.encodeMaybe r.player.side )
                    , ( "last_event", E.int r.lastEventId )
                    ]
                )
        , expect = Http.expectJson r.toMsg decodeUpdate
        }


ping :
    { gameId : String
    , player : Player
    , toMsg : Result Http.Error () -> msg
    , client : Client
    }
    -> Cmd msg
ping r =
    Http.post
        { url = endpointUrl r.client.baseUrl "/ping"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string r.gameId )
                    , ( "player_id", E.string r.player.id )
                    , ( "name", E.string r.player.name )
                    , ( "team", Side.encodeMaybe r.player.side )
                    ]
                )
        , expect = Http.expectWhatever r.toMsg
        }


longPollEvents :
    { gameId : String
    , player : Player
    , lastEventId : Int
    , toMsg : Result Http.Error Update -> msg
    , tracker : String
    , client : Client
    }
    -> Cmd msg
longPollEvents r =
    Http.request
        { method = "POST"
        , headers = []
        , url = endpointUrl r.client.baseUrl "/events"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string r.gameId )
                    , ( "player_id", E.string r.player.id )
                    , ( "name", E.string r.player.name )
                    , ( "team", Side.encodeMaybe r.player.side )
                    , ( "last_event", E.int r.lastEventId )
                    ]
                )
        , expect = Http.expectJson r.toMsg decodeUpdate
        , timeout = Just 45000
        , tracker = Just r.tracker
        }


maybeMakeGame :
    { gameId : String
    , prevSeed : Maybe String
    , toMsg : Result Http.Error GameState -> msg
    , client : Client
    }
    -> Cmd msg
maybeMakeGame r =
    Http.post
        { url = endpointUrl r.client.baseUrl "/new-game"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "game_id", E.string r.gameId )
                    , ( "prev_seed"
                      , case r.prevSeed of
                            Nothing ->
                                E.null

                            Just seed ->
                                E.string seed
                      )
                    ]
                )
        , expect = Http.expectJson r.toMsg decodeGameState
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
