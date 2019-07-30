module App.View exposing (view)

import App.Model exposing (Model)
import App.Msg exposing (Msg)
import Html as H exposing (Html)


view : Model -> Html Msg
view model =
    H.div [] [ H.text "Hello world!" ]
