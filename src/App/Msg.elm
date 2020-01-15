module App.Msg exposing (Msg(..))

import Grid
import Location exposing (Location)
import Variant exposing (Variant)


type Msg
    = Tick Int
    | Fill (List Variant)
    | Grid Grid.Msg
    | Kill Location
    | TogglePause
    | Reset
