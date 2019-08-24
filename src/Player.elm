module Player exposing (Player)

import Side


type alias Player =
    { id : String
    , name : String
    , side : Maybe Side.Side
    }
