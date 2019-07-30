module App.Init exposing (init)

import App.Model exposing (Model)
import App.Msg exposing (Msg)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )
