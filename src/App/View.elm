module App.View exposing (view)

import App.Model exposing (Model)
import App.Msg exposing (Msg(..))
import Dict
import Grid exposing (Grid)
import Html as H exposing (Html)
import Html.Events as HE
import Item exposing (Item(..))
import Location exposing (Location)
import Svg as S exposing (Svg)
import Svg.Attributes as A
import Svg.Events as E
import Util exposing (iif)
import Variant


view : Model -> Html Msg
view { grid, paused } =
    H.div []
        [ viewGrid False False grid
        , H.button [ HE.onClick TogglePause ] [ H.text <| "Pause " ++ iif paused "On" "Off" ]
        , H.button [ HE.onClick Reset ] [ H.text "Reset" ]
        ]


{-| view
-}
viewGrid : Bool -> Bool -> Grid -> Svg Msg
viewGrid debug labels { items, size, config } =
    let
        { cols, rows } =
            size

        { itemSize, gap } =
            config

        width =
            gap + ((itemSize + gap) * cols)

        playHeight =
            (itemSize + gap) * rows

        height =
            playHeight * 2 + gap

        usedHeight =
            if debug then
                height

            else
                playHeight

        viewBox =
            if debug then
                [ 0, 0, width, height ]
                    |> List.map String.fromInt
                    |> List.intersperse " "
                    |> String.concat

            else
                [ 0, playHeight, width, playHeight ]
                    |> List.map String.fromInt
                    |> List.intersperse " "
                    |> String.concat

        positionsRender =
            if labels then
                List.map (viewPosition itemSize gap rows) (Dict.keys items)

            else
                []
    in
    S.svg
        [ A.width <| String.fromInt width
        , A.height <| String.fromInt usedHeight
        , A.viewBox viewBox
        ]
        ([]
            ++ positionsRender
            ++ List.map (viewItem itemSize gap) (Dict.toList items)
        )


viewPosition : Int -> Int -> Int -> Location -> Svg Msg
viewPosition size gap rows location =
    let
        x =
            Location.svgX size gap location

        y =
            Location.svgY size gap location

        text =
            "("
                ++ String.fromInt (Location.row location)
                ++ ", "
                ++ String.fromInt (Location.column location)
                ++ ")"

        color =
            if Location.row location < rows then
                "pink"

            else
                "gray"
    in
    S.g []
        [ S.text_
            [ A.x <| String.fromInt (x + size // 2)
            , A.y <| String.fromInt (y + size // 2)
            , A.dominantBaseline "middle"
            , A.textAnchor "middle"
            ]
            [ S.text text ]
        , S.rect
            [ A.width <| String.fromInt size
            , A.height <| String.fromInt size
            , A.x <| String.fromInt x
            , A.y <| String.fromInt y
            , A.fill color
            , A.opacity "0.3"
            ]
            []
        ]


viewItem : Int -> Int -> ( Location, Item ) -> Svg Msg
viewItem size gap ( location, item ) =
    let
        ( x, y ) =
            case item of
                MovingFrom _ ( cx, cy ) _ _ ->
                    ( cx, cy )

                _ ->
                    ( Location.svgX size gap location, Location.svgY size gap location )

        color =
            case item of
                Variant variant ->
                    Variant.toString variant

                MovingFrom _ _ _ variant ->
                    Variant.toString variant

                NeedsVariant ->
                    "pink"

                Empty ->
                    "transparent"

                Exploding _ _ variant ->
                    Variant.toString variant

        opacity =
            case item of
                Exploding _ progress _ ->
                    1 - progress

                _ ->
                    1
    in
    S.g []
        [ S.rect
            [ A.width <| String.fromInt size
            , A.height <| String.fromInt size
            , A.x <| String.fromInt x
            , A.y <| String.fromInt y
            , A.fill color
            , A.opacity <| String.fromFloat opacity
            , E.onClick <| Kill location
            ]
            []
        ]
