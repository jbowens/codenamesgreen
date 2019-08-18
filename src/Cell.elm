module Cell exposing (Cell, Display(..), display, isExposed, sideColor, tapped)

import Color
import Side


type alias Cell =
    { index : Int
    , word : String
    , a : ( Bool, Color.Color )
    , b : ( Bool, Color.Color )
    }


type Display
    = ExposedGreen
    | ExposedBlack
    | Hidden Bool Bool


display : Cell -> Display
display cell =
    case ( cell.a, cell.b ) of
        ( ( True, Color.Black ), _ ) ->
            ExposedBlack

        ( _, ( True, Color.Black ) ) ->
            ExposedBlack

        ( ( True, Color.Green ), _ ) ->
            ExposedGreen

        ( _, ( True, Color.Green ) ) ->
            ExposedGreen

        ( ( exposedA, _ ), ( exposedB, _ ) ) ->
            Hidden exposedA exposedB


sideColor : Side.Side -> Cell -> Color.Color
sideColor side cell =
    case side of
        Side.A ->
            Tuple.second cell.a

        Side.B ->
            Tuple.second cell.b

        -- TODO: make Side just A and B
        _ ->
            Color.Black


tapped : Side.Side -> Cell -> Cell
tapped side cell =
    case side of
        Side.B ->
            { cell | a = ( True, Tuple.second cell.a ) }

        Side.A ->
            { cell | b = ( True, Tuple.second cell.b ) }

        _ ->
            cell


isExposed : Side.Side -> Cell -> Bool
isExposed side cell =
    case side of
        Side.A ->
            Tuple.first cell.a

        Side.B ->
            Tuple.first cell.b

        Side.None ->
            False
