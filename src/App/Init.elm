module App.Init exposing (init)

import App.Model exposing (Model)
import App.Msg exposing (Msg(..))
import Config exposing (config, size)
import Grid exposing (Config, Size)
import Random
import Variant


init : () -> ( Model, Cmd Msg )
init _ =
    let
        grid =
            Grid.init config size
    in
    ( Model grid False, Random.generate Fill (Variant.random (size.rows * size.cols)) )
