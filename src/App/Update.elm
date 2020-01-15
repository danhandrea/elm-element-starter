module App.Update exposing (update)

import App.Model exposing (Model)
import App.Msg exposing (Msg(..))
import Config exposing (config, size)
import Grid
import Process
import Random
import Task
import Time exposing (Posix)
import Util exposing (iif)
import Variant exposing (Variant)


tickSize =
    50


update : Msg -> Model -> ( Model, Cmd Msg )
update message ({ paused } as model) =
    case message of
        TogglePause ->
            ( { model | paused = not paused }, Cmd.none )

        Reset ->
            let
                grid =
                    Grid.init config size
            in
            ( { model | grid = grid }, Random.generate Fill (Variant.random (size.rows * size.cols)) )

        Tick time ->
            let
                ( newGrid, cmd ) =
                    Grid.tick time model.grid
            in
            ( { model | grid = newGrid }
            , Cmd.batch
                [ Cmd.map Grid cmd
                ]
            )

        Fill variants ->
            ( { model | grid = Grid.fill variants model.grid }, Cmd.none )

        Grid msg ->
            case msg of
                Grid.Refill n ->
                    ( model, Random.generate Fill (Variant.random n) )

        Kill location ->
            ( { model | grid = Grid.kill location model.grid }, Cmd.none )


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)
