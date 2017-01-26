module Model exposing (..)

import Tile exposing (Tile(..), TileGrid)
import Point exposing (Point)


type alias Model =
    { width : Int
    , height : Int
    , position : Point
    , isTicking : Bool
    , data : List Int
    , tiles : TileGrid
    , selectedTile : Tile
    }


initState =
    { width = 8
    , height = 8
    , position = ( 0, 0 )
    , isTicking = False
    , data = [ 1, 2, 3 ]
    , tiles = Tile.makeGrid 8 8
    , selectedTile = EmptyTile
    }


init =
    ( initState, Cmd.none )


hasToken model point =
    model.position == point


area model =
    Tile.count model.tiles


tileAt model point =
    Tile.tileAt point model.tiles
