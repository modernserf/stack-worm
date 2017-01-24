module Tile exposing (Tile(..), TileGrid, makeGrid, tileAt, count, replaceAt, updateStack)

import Dict exposing (Dict)


type Tile
    = EmptyTile
    | Draw
    | Drop


type alias Point =
    ( Int, Int )


type alias TileGrid =
    Dict Point Tile



-- tile grid


mapTo x f =
    List.map f <| List.range 0 <| x - 1


makeGrid width height =
    (mapTo height (\y -> mapTo width (\x -> ( ( x, y ), EmptyTile ))))
        |> List.concat
        |> Dict.fromList


(<<<) =
    (<<) << (<<)


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

        EmptyTile ->
            identity


dup stack =
    case List.head stack of
        Just head ->
            head :: stack

        Nothing ->
            []


drop =
    Maybe.withDefault [] << List.tail
