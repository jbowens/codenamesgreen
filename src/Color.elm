module Color exposing (Color(..), decode, toString)

import Json.Decode


type Color
    = Tan
    | Green
    | Black


toString : Color -> String
toString color =
    case color of
        Tan ->
            "tan"

        Green ->
            "green"

        Black ->
            "black"


decode : Json.Decode.Decoder Color
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case s of
                    "t" ->
                        Json.Decode.succeed Tan

                    "g" ->
                        Json.Decode.succeed Green

                    "b" ->
                        Json.Decode.succeed Black

                    _ ->
                        Json.Decode.fail "unrecognized color"
            )
