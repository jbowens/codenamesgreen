port module User exposing (User, decode, storeUpdatedUser)

import Json.Decode as D
import Json.Encode as E


port storeCache : E.Value -> Cmd msg


{-| A user holds persisted user preferences.

It's stored in local storage, and is used to
keep settings like the player's name between
sessions.

-}
type alias User =
    { playerId : String
    , name : String
    }


storeUpdatedUser : User -> Cmd msg
storeUpdatedUser user =
    user
        |> encode
        |> storeCache


decode : D.Value -> Result D.Error User
decode value =
    D.decodeValue D.string value
        |> Result.andThen (D.decodeString decoder)


encode : User -> E.Value
encode user =
    E.object
        [ ( "player_id", E.string user.playerId )
        , ( "name", E.string user.name )
        ]


decoder : D.Decoder User
decoder =
    D.map2 User
        (D.field "player_id" D.string)
        (D.field "name" D.string)
