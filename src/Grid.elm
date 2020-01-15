module Grid exposing (Config, Grid, Msg(..), Size, fill, init, kill, tick)

import Dict exposing (Dict)
import Duration
import Ease
import Item exposing (Item(..))
import Location exposing (Location)
import Task
import Variant exposing (Variant)



-- MODEL


type alias Grid =
    { config : Config
    , size : Size
    , items : Dict Location Item
    }



-- MSG


type Msg
    = Refill Int



-- INIT


init : Config -> Size -> Grid
init config ({ rows, cols } as size) =
    let
        items =
            ([]
                ++ List.repeat (rows * cols) NeedsVariant
                ++ List.repeat (rows * cols) Empty
            )
                |> List.indexedMap (\index item -> ( Location.from cols index, item ))
                |> Dict.fromList
    in
    Grid config size items


tick : Int -> Grid -> ( Grid, Cmd Msg )
tick timestamp grid =
    let
        moved =
            move timestamp grid

        updatedMovements =
            updateMovements timestamp moved

        fc =
            firstChain updatedMovements

        killed =
            case fc of
                Nothing ->
                    updatedMovements

                Just chain ->
                    explode timestamp chain updatedMovements

        needRefill =
            killed.items
                |> Dict.filter (\_ item -> item == NeedsVariant)
                |> Dict.size
    in
    ( killed
    , if needRefill > 0 then
        do <| Refill needRefill

      else
        Cmd.none
    )


kill : Location -> Grid -> Grid
kill location ({ items, size } as grid) =
    { grid
        | items =
            Dict.update location
                (\_ ->
                    if Location.row location < size.rows then
                        Just NeedsVariant

                    else
                        Just Empty
                )
                items
    }


explode : Int -> List Location -> Grid -> Grid
explode timestamp chain ({ items } as grid) =
    { grid
        | items =
            items
                |> Dict.map
                    (\location item ->
                        case item of
                            Variant variant ->
                                if List.member location chain then
                                    Exploding timestamp 0 variant

                                else
                                    item

                            _ ->
                                item
                    )
    }


killList : List Location -> Grid -> Grid
killList chain ({ items } as grid) =
    { grid
        | items =
            items
                |> Dict.map
                    (\location item ->
                        if List.member location chain then
                            Empty

                        else
                            item
                    )
    }


rowsIndex : Int -> ( Int, Int ) -> Int
rowsIndex cols ( column, index ) =
    index * cols + column


colsIndex : Int -> ( Int, Int ) -> Int
colsIndex rows ( column, index ) =
    index + rows * column


firstChain : Grid -> Maybe (List Location)
firstChain { items, size } =
    let
        pairs =
            items
                |> Dict.filter (\location _ -> Location.row location >= size.rows)
                |> Dict.toList

        horizontal ( pa, a ) ( pb, b ) =
            a == b && a /= Empty && Location.row pa == Location.row pb

        vertical ( pa, a ) ( pb, b ) =
            a == b && a /= Empty && Location.column pa == Location.column pb

        horizontalChain =
            pairs
                |> List.sortBy (\( location, _ ) -> rowsIndex size.cols ( Location.column location, Location.row location ))
                |> List.reverse
                |> group_ [] horizontal
                |> List.filter (\a -> List.length a > 2)
                |> List.head

        verticalChain =
            pairs
                |> List.sortBy (\( location, _ ) -> colsIndex size.rows ( Location.column location, Location.row location ))
                |> List.reverse
                |> group_ [] vertical
                |> List.filter (\a -> List.length a > 2)
                |> List.head

        horizontalChainMaxRow =
            case horizontalChain of
                Nothing ->
                    0

                Just chain ->
                    chain
                        |> List.map (\( location, _ ) -> Location.row location)
                        |> List.maximum
                        |> Maybe.withDefault 0

        verticalChainMaxRow =
            case verticalChain of
                Nothing ->
                    0

                Just chain ->
                    chain
                        |> List.map (\( location, _ ) -> Location.row location)
                        |> List.maximum
                        |> Maybe.withDefault 0

        lowest =
            if verticalChainMaxRow > horizontalChainMaxRow then
                verticalChain

            else
                horizontalChain

        result =
            case lowest of
                Nothing ->
                    Nothing

                Just list ->
                    Just
                        (list
                            |> List.map (\( location, _ ) -> location)
                        )
    in
    result


do : msg -> Cmd msg
do msg =
    Task.succeed msg
        |> Task.perform identity


move : Int -> Grid -> Grid
move timestamp ({ items, config } as grid) =
    let
        moving =
            items
                |> Dict.filter
                    (\_ item ->
                        case item of
                            MovingFrom _ _ _ _ ->
                                True

                            Exploding _ _ _ ->
                                True

                            _ ->
                                False
                    )

        moved =
            moving
                |> Dict.map
                    (\location1 item ->
                        case item of
                            MovingFrom location0 ( x, _ ) timestamp0 variant ->
                                let
                                    duration =
                                        Duration.duration config.itemSize config.gap location0 location1

                                    progress =
                                        clamp 0 100 <|
                                            (toFloat (timestamp - timestamp0) / toFloat duration)
                                                * toFloat 100

                                    y0 =
                                        Location.svgY config.itemSize config.gap location0

                                    y1 =
                                        Location.svgY config.itemSize config.gap location1

                                    eased =
                                        Ease.inCubic (progress / 100)

                                    dy =
                                        toFloat (y1 - y0)

                                    y =
                                        clamp y0 y1 <| y0 + (round <| dy * eased)
                                in
                                if progress >= 100 then
                                    Variant variant

                                else
                                    MovingFrom location0 ( x, y ) timestamp0 variant

                            Exploding timestamp0 _ variant ->
                                let
                                    duration =
                                        500

                                    progress =
                                        clamp 0 1 <|
                                            (toFloat (timestamp - timestamp0) / toFloat duration)
                                in
                                if progress >= 1 then
                                    Empty

                                else
                                    Exploding timestamp0 progress variant

                            _ ->
                                item
                    )

        new =
            Dict.union moved items
    in
    { grid | items = new }


updateMovements : Int -> Grid -> Grid
updateMovements timestamp ({ items, config } as grid) =
    let
        groupedLocations =
            items
                |> Dict.keys
                |> List.sortBy Location.column
                |> group_ [] (\a b -> Location.column a == Location.column b)
                |> List.map (\column -> column |> List.sortBy Location.row |> List.reverse)

        groupedItems =
            items
                |> Dict.filter (\_ item -> item /= Empty)
                |> Dict.toList
                |> List.sortBy (\( location, _ ) -> Location.column location)
                |> group_ [] (\( locationA, _ ) ( locationB, _ ) -> Location.column locationA == Location.column locationB)
                |> List.map (\column -> column |> List.sortBy (\( location, _ ) -> Location.row location) |> List.reverse)

        pairs =
            List.map2 (\locationColumn itemColumn -> List.map2 Tuple.pair locationColumn itemColumn) groupedLocations groupedItems
                |> List.concat

        new =
            pairs
                |> List.concatMap
                    (\( locA, ( locB, item ) ) ->
                        if locA == locB then
                            []

                        else
                            let
                                svgLocB =
                                    Location.svg config.itemSize config.gap locB
                            in
                            case item of
                                Variant variant ->
                                    [ ( locA, MovingFrom locB svgLocB timestamp variant ), ( locB, NeedsVariant ) ]

                                _ ->
                                    []
                    )
                |> Dict.fromList

        newItems =
            Dict.union new items
    in
    { grid | items = newItems }


fill : List Variant -> Grid -> Grid
fill variants ({ items } as grid) =
    let
        needsVariant =
            items
                |> Dict.filter (\_ item -> item == NeedsVariant)
                |> Dict.toList

        withVariant =
            List.map2
                (\( loc, _ ) variant ->
                    ( loc, Variant variant )
                )
                needsVariant
                variants
                |> Dict.fromList

        newItems =
            Dict.union withVariant items
    in
    { grid | items = newItems }


group_ : List (List a) -> (a -> a -> Bool) -> List a -> List (List a)
group_ acc condition list =
    case list of
        [] ->
            List.reverse acc

        a :: bs ->
            case acc of
                [] ->
                    group_ [ [ a ] ] condition bs

                (y :: ys) :: xs ->
                    if condition a y then
                        group_ ((a :: y :: ys) :: xs) condition bs

                    else
                        group_ ([ a ] :: (y :: ys) :: xs) condition bs

                [] :: _ ->
                    acc


type alias Config =
    { itemSize : Int
    , gap : Int
    }


type alias Size =
    { cols : Int
    , rows : Int
    }
