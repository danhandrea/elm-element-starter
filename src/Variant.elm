module Variant exposing (Variant, random, toString)

import Random exposing (Generator)


type Variant
    = Red
    | Green
    | Blue
    | Orange


random_ : Generator Variant
random_ =
    Random.uniform Red [ Green, Blue ]


random : Int -> Random.Generator (List Variant)
random n =
    Random.list n random_


toString : Variant -> String
toString variant =
    case variant of
        Red ->
            "red"

        Green ->
            "green"

        Blue ->
            "blue"

        Orange ->
            "orange"
