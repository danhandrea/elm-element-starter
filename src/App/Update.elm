module App.Update exposing (update)

import App.Model exposing (Model)
import App.Msg exposing (Msg(..))


update : Msg -> Model -> Model
update msg model =
    model
