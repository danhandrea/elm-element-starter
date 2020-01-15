module App.Model exposing (Model)

import Grid exposing (Grid)


type alias Model =
    { grid : Grid
    , paused : Bool
    }
