module Side exposing (Side(..), decode, encode, opposite, toString)

import Json.Decode
import Json.Encode


type Side
    = None
    | A
    | B


toString : Side -> String
toString side =
    case side of
        None ->
            "none"

        A ->
            "A"

        B ->
            "B"


encode : Side -> Json.Encode.Value
encode side =
    Json.Encode.int
        (case side of
            A ->
                1

            B ->
                2

            None ->
                0
        )


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
                        Json.Decode.succeed None
            )


opposite : Side -> Side
opposite side =
    case side of
        None ->
            None

        A ->
            B

        B ->
            A
