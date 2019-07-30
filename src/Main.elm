module Main exposing (main)

import App.Init as App
import App.Subscriptions as App
import App.Update as App
import App.View as App
import Browser


main =
    Browser.element
        { init = App.init
        , update = App.update
        , view = App.view
        , subscriptions = App.subscriptions
        }
