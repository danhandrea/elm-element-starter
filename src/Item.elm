module Item exposing (Item(..))

import Location exposing (Location)
import Variant exposing (Variant)


type Item
    = Variant Variant
    | MovingFrom Location Location Int Variant
    | NeedsVariant
    | Empty
    | Exploding Int Float Variant
