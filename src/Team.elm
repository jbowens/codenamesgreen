module Team exposing (Team(..), decode, encode, opposite, toString)

import Json.Decode
import Json.Encode


type Team
    = None
    | A
    | B


toString : Team -> String
toString team =
    case team of
        None ->
            "none"

        A ->
            "A"

        B ->
            "B"


encode : Team -> Json.Encode.Value
encode team =
    Json.Encode.int
        (case team of
            A ->
                1

            B ->
                2

            None ->
                0
        )


decode : Json.Decode.Decoder Team
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


opposite : Team -> Team
opposite team =
    case team of
        None ->
            None

        A ->
            B

        B ->
            A
