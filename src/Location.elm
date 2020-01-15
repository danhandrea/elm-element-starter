module Location exposing (Location, column, from, row, svg, svgX, svgY)

{-| -}


{-| Grid location

    ( row, column )

-}
type alias Location =
    ( Int, Int )


{-| from

Initialize a location from the number of
columns in a grid and the index

    from 3 1 =
        Location ( 0, 1 )
    from 3 5 =
        Location ( 1, 2 )

-}
from : Int -> Int -> Location
from columns index =
    ( index // columns, modBy columns index )


{-| Location column
-}
column : Location -> Int
column ( _, c ) =
    c


{-| Location row
-}
row : Location -> Int
row ( r, _ ) =
    r


svg : Int -> Int -> Location -> Location
svg size gap location =
    ( svgX size gap location, svgY size gap location )


svgX : Int -> Int -> Location -> Int
svgX size gap location =
    gap + ((size + gap) * column location)


svgY : Int -> Int -> Location -> Int
svgY size gap location =
    gap + ((size + gap) * row location)
