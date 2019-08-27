module Player exposing (Player)

import Side
import User


type alias Player =
    { user : User.User
    , side : Maybe Side.Side
    }
