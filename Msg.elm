module Msg exposing (..)

import Tile exposing (Tile)
import Point exposing (Point)


type Msg
    = Tick
    | ToggleTicking
    | SelectedTile Tile
    | PlacedTile Point
