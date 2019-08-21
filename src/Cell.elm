module Cell exposing (Cell, Display(..), display, isExposed, sideColor, tapped, view)

import Color
import Html exposing (Html, div, i, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
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


tapped : Side.Side -> Cell -> Cell
tapped side cell =
    case side of
        Side.B ->
            { cell | a = ( True, Tuple.second cell.a ) }

        Side.A ->
            { cell | b = ( True, Tuple.second cell.b ) }


isExposed : Side.Side -> Cell -> Bool
isExposed side cell =
    case side of
        Side.A ->
            Tuple.first cell.a

        Side.B ->
            Tuple.first cell.b


view : Maybe Side.Side -> (Cell -> a) -> Cell -> Html a
view viewerSide msg cell =
    case display cell of
        ExposedGreen ->
            div [ Attr.class "cell", Attr.class "green" ] [ text cell.word ]

        ExposedBlack ->
            div [ Attr.class "cell", Attr.class "black" ] [ text cell.word ]

        Hidden guessedA guessedB ->
            let
                pickable =
                    viewerSide
                        |> Maybe.map (\side -> (side == Side.A && not guessedB) || (side == Side.B && not guessedA))
                        |> Maybe.withDefault False
            in
            div
                (condList
                    [ ( Attr.class "cell", True )
                    , ( Attr.class "pickable", pickable )
                    , ( onClick (msg cell), pickable )
                    ]
                )
                (condList
                    [ ( i [ Attr.class "icon time-token-a", Attr.class (timeTokenIcon viewerSide Side.B) ] [], guessedA )
                    , ( i [ Attr.class "icon time-token-b", Attr.class (timeTokenIcon viewerSide Side.A) ] [], guessedB )
                    , ( div [ Attr.class "word" ] [ text cell.word ], True )
                    ]
                )


timeTokenIcon : Maybe Side.Side -> Side.Side -> String
timeTokenIcon viewerSide side =
    case viewerSide of
        Nothing ->
            "ion-ios-close-circle"

        Just s ->
            if s == side then
                "ion-ios-arrow-dropdown-circle"

            else
                "ion-ios-arrow-dropup-circle"


condList : List ( a, Bool ) -> List a
condList list =
    list
        |> List.filter Tuple.second
        |> List.map Tuple.first
