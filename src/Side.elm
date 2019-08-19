module Side exposing (Side(..), decode, decodeMaybe, encode, encodeMaybe, opposite, toString)

import Json.Decode
import Json.Encode


type Side
    = A
    | B


toString : Side -> String
toString side =
    case side of
        A ->
            "A"

        B ->
            "B"


opposite : Side -> Side
opposite side =
    case side of
        A ->
            B

        B ->
            A


encode : Side -> Json.Encode.Value
encode side =
    Json.Encode.int
        (case side of
            A ->
                1

            B ->
                2
        )


encodeMaybe : Maybe Side -> Json.Encode.Value
encodeMaybe m =
    case m of
        Just s ->
            encode s

        Nothing ->
            Json.Encode.int 0


decode : Json.Decode.Decoder Side
decode =
    Json.Decode.int
        |> Json.Decode.andThen
            (\i ->
                case i of
                    1 ->
                        Json.Decode.succeed A

                    2 ->
                        Json.Decode.succeed B

                    _ ->
                        Json.Decode.fail "unknown side"
            )


decodeMaybe : Json.Decode.Decoder (Maybe Side)
decodeMaybe =
    Json.Decode.int
        |> Json.Decode.andThen
            (\i ->
                case i of
                    1 ->
                        Json.Decode.succeed (Just A)

                    2 ->
                        Json.Decode.succeed (Just B)

                    _ ->
                        Json.Decode.succeed Nothing
            )
