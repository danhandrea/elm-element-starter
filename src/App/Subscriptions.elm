module App.Subscriptions exposing (subscriptions)

import App.Model exposing (Model)
import App.Msg exposing (Msg(..))
import Browser.Events exposing (onAnimationFrame)
import Time
import Util exposing (iif)


subscriptions : Model -> Sub Msg
subscriptions { paused } =
    iif paused Sub.none (onAnimationFrame (Time.posixToMillis >> Tick))



--
