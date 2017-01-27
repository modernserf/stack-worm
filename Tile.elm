module Tile exposing (Tile(..), TileGrid, makeGrid, tileAt, count, replaceAt, updateStack)

import Dict exposing (Dict)
import Point exposing (Point)
import Util exposing (minRange, cartesian, (<<<))


type Tile
    = EmptyTile
    | Draw
    | Drop
    | Swap
    | Rot


type alias TileGrid =
    Dict Point Tile



-- tile grid


makeGrid width height =
    cartesian (minRange width) (minRange height)
        |> List.map (flip (,) EmptyTile)
        |> Dict.fromList


tileAt =
    Maybe.withDefault EmptyTile <<< Dict.get


count =
    Dict.size


replaceAt tiles point value =
    case Dict.get point tiles of
        Just _ ->
            Dict.insert point value tiles

        Nothing ->
            tiles



-- stack effects


updateStack tile =
    case tile of
        Draw ->
            dup

        Drop ->
            drop

        Swap ->
            swap

        Rot ->
            rot

        _ ->
            identity


dup stack =
    case List.head stack of
        Just head ->
            head :: stack

        Nothing ->
            []


drop =
    Maybe.withDefault [] << List.tail


split index list =
    ( List.take index list, List.drop index list )


swap stack =
    let
        ( top, rest ) =
            split 2 stack
    in
        (List.reverse top) ++ rest


rot stack =
    let
        ( top, rest ) =
            split 3 stack

        ( front, back ) =
            split 2 top
    in
        back ++ front ++ rest
