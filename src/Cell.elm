module Cell exposing (Cell, Display(..), display, isExposed, side, tapped)

import Color
import Team


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


side : Team.Team -> Cell -> Color.Color
side team cell =
    case team of
        Team.A ->
            Tuple.second cell.a

        Team.B ->
            Tuple.second cell.b

        -- TODO: make Team just A and B
        _ ->
            Color.Black


tapped : Team.Team -> Cell -> Cell
tapped team cell =
    case team of
        Team.B ->
            { cell | a = ( True, Tuple.second cell.a ) }

        Team.A ->
            { cell | b = ( True, Tuple.second cell.b ) }

        _ ->
            cell


isExposed : Team.Team -> Cell -> Bool
isExposed team cell =
    case team of
        Team.A ->
            Tuple.first cell.a

        Team.B ->
            Tuple.first cell.b

        Team.None ->
            False
