module View exposing (view)

import Html as H
import Html.Events as E
import Html.Attributes
import Css exposing (..)
import Model exposing (..)
import Msg exposing (Msg(..))
import Tile exposing (Tile(..))
import Util exposing (minRange, (<<<))


view model =
    H.div []
        [ grid model
        , stack model
        , tileButtons model
        , toggleButton model
        ]


styles =
    Css.asPairs >> Html.Attributes.style



-- grid


toggleLabel model =
    if model.isTicking then
        "Stop"
    else
        "Go"


toggleButton model =
    H.button [ E.onClick ToggleTicking ] [ H.text <| toggleLabel model ]


mapRange_ el max f =
    List.map (\x -> el [] (f x)) (minRange max)


mapRange =
    (>>) minRange
        << (>>) (flip List.map)
        << flip ((<<) <| (<<) (>>) (<<)) []


grid model =
    H.table []
        (mapRange H.tr
            model.height
            (\y ->
                (mapRange H.td
                    model.width
                    (\x -> cell model ( x, y ))
                )
            )
        )


cellStyle =
    styles
        [ border3 (px 1) solid (rgb 100 100 100)
        , width <| px 64
        , height <| px 64
        , textAlign center
        , fontSize <| px 32
        ]


cell model pos =
    [ if hasToken model pos then
        H.text "X"
      else
        H.div
            [ cellStyle, E.onClick <| PlacedTile pos ]
            [ H.text <| drawTile <| tileAt model pos ]
    ]


drawTile tile =
    case tile of
        Draw ->
            "+"

        Drop ->
            "-"

        Swap ->
            "🔀"

        Rot ->
            "🔄"

        EmptyTile ->
            " "


stack model =
    H.div [] <| List.map (H.text << toString) model.data


tileOptions =
    [ EmptyTile, Draw, Drop, Swap, Rot ]


tileButtons model =
    H.div [] <| List.map (tileButton model) tileOptions


tileButton model tile =
    H.button [ E.onClick <| SelectedTile tile ] [ H.text <| drawTile tile ]
