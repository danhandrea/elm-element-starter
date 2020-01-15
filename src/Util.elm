module Util exposing (iif)


iif : Bool -> a -> a -> a
iif condition first second =
    if condition then
        first

    else
        second
