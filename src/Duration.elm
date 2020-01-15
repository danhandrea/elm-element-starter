module Duration exposing (duration)

import Location exposing (Location)


distance : Int -> Int -> Location -> Location -> Int
distance size gap from to =
    let
        fromY =
            Location.svgY size gap from

        toY =
            Location.svgY size gap to
    in
    toY - fromY


speed : Float
speed =
    0.5


duration : Int -> Int -> Location -> Location -> Int
duration size gap from to =
    let
        dist =
            distance size gap from to
    in
    round <| toFloat dist / speed
